#![allow(unused)] // temporary

mod slash_choice_parameter;

use proc_macro::TokenStream;
use syn::spanned::Spanned as _;

// Convert None => None and Some(T) => Some(T)
fn wrap_option<T: quote::ToTokens>(literal: Option<T>) -> syn::Expr {
    match literal {
        Some(literal) => syn::parse_quote! { Some(#literal) },
        None => syn::parse_quote! { None },
    }
}

struct AllLifetimesToStatic;
impl syn::fold::Fold for AllLifetimesToStatic {
    fn fold_lifetime(&mut self, _: syn::Lifetime) -> syn::Lifetime {
        syn::parse_quote! { 'static }
    }
}

type Error = darling::Error;

#[derive(Debug, Default)]
struct Aliases(Vec<String>);

impl darling::FromMeta for Aliases {
    fn from_list(items: &[::syn::NestedMeta]) -> darling::Result<Self> {
        items
            .iter()
            .map(|item| String::from_nested_meta(item))
            .collect::<darling::Result<Vec<String>>>()
            .map(Self)
    }
}

// #[derive(Debug, darling::FromMeta)]
// struct BroadcastTypingArgs {
//     #[darling(default)]
//     after: f32,
// }

/// Representation of the command attribute arguments (`#[command(...)]`)
#[derive(Default, Debug, darling::FromMeta)]
#[darling(default)]
struct CommandAttrArgs {
    aliases: Aliases,
    track_edits: bool,
    // broadcast_typing: Option<BroadcastTypingArgs>,
    broadcast_typing: Option<()>,
    defer_response: Option<bool>,
    explanation_fn: Option<syn::Path>,
    check: Option<syn::Path>,
    on_error: Option<syn::Path>,
    rename: Option<String>,
    discard_spare_arguments: bool,
    slash_command: bool,
    hide_in_help: bool,
    ephemeral: bool,
    required_permissions: Option<syn::Ident>,
    owners_only: bool,
}

/// Representation of the function parameter attribute arguments
#[derive(Default, Debug, darling::FromMeta)]
#[darling(default)]
struct ParamAttrArgs {
    description: Option<String>,
    lazy: bool,
    flag: bool,
    rest: bool,
}

/// Part of the Invocation struct. Represents a single parameter of a Discord command.
struct CommandParameter {
    name: syn::Ident,
    type_: syn::Type,
    more: ParamAttrArgs,
    span: proc_macro2::Span,
}

/// Passed to prefix and slash command spec generators; contains info to be included in command spec
struct Invocation<'a> {
    command_name: String,
    ctx_type: &'a syn::Type,
    return_type: &'a syn::Type,
    parameters: &'a [CommandParameter],
    description: Option<&'a str>,
    explanation: Option<&'a str>,
    function: &'a syn::ItemFn,
    required_permissions: &'a syn::Expr,
    more: &'a CommandAttrArgs,
}

fn extract_help_from_doc_comments(attrs: &[syn::Attribute]) -> (Option<String>, Option<String>) {
    let mut doc_lines = String::new();
    for attr in attrs {
        if attr.path == quote::format_ident!("doc").into() {
            for token in attr.tokens.clone() {
                if let Ok(literal) = syn::parse2::<syn::LitStr>(token.into()) {
                    let literal = literal.value();
                    let literal = literal.strip_prefix(' ').unwrap_or(&literal);

                    doc_lines += literal;
                    doc_lines += "\n";
                }
            }
        }
    }

    // Apply newline escapes
    let doc_lines = doc_lines.trim().replace("\\\n", "");

    if doc_lines.is_empty() {
        return (None, None);
    }

    let mut paragraphs = doc_lines.splitn(2, "\n\n");
    let inline_help = paragraphs.next().unwrap().replace("\n", " ");
    let multiline_help = paragraphs.next().map(|x| x.to_owned());

    (Some(inline_help), multiline_help)
}

// ngl this is ugly
// transforms a type of form `OuterType<T>` into `T`
fn extract_type_parameter<'a>(outer_type: &str, t: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(path) = t {
        if path.path.segments.len() == 1 {
            let path = &path.path.segments[0];
            if path.ident == outer_type {
                if let syn::PathArguments::AngleBracketed(generics) = &path.arguments {
                    if generics.args.len() == 1 {
                        if let syn::GenericArgument::Type(t) = &generics.args[0] {
                            return Some(t);
                        }
                    }
                }
            }
        }
    }
    None
}

fn extract_option_type(t: &syn::Type) -> Option<&syn::Type> {
    extract_type_parameter("Option", t)
}

fn extract_vec_type(t: &syn::Type) -> Option<&syn::Type> {
    extract_type_parameter("Vec", t)
}

fn generate_prefix_command_spec(inv: &Invocation) -> Result<proc_macro2::TokenStream, Error> {
    let description = wrap_option(inv.description);
    let explanation = match &inv.more.explanation_fn {
        Some(explanation_fn) => quote::quote! { Some(#explanation_fn) },
        None => match &inv.explanation {
            Some(extracted_explanation) => quote::quote! { Some(|| #extracted_explanation.into()) },
            None => quote::quote! { None },
        },
    };

    // Box::pin the check and on_error callbacks in order to store them in a struct
    let check = match &inv.more.check {
        Some(check) => {
            if inv.more.slash_command {
                quote::quote! { Some(|ctx| Box::pin(#check(::poise::Context::Prefix(ctx)))) }
            } else {
                quote::quote! { Some(|ctx| Box::pin(#check(ctx))) }
            }
        }
        None => quote::quote! { None },
    };
    let on_error = match &inv.more.on_error {
        Some(on_error) => {
            if inv.more.slash_command {
                quote::quote! {
                    Some(|err, ctx| Box::pin(#on_error(err, ::poise::CommandErrorContext::Prefix(ctx))))
                }
            } else {
                quote::quote! { Some(|err, ctx| Box::pin(#on_error(err, ctx))) }
            }
        }
        None => quote::quote! { None },
    };

    let maybe_wrapped_ctx = if inv.more.slash_command {
        quote::quote! { ::poise::Context::Prefix(ctx) }
    } else {
        quote::quote! { ctx }
    };

    let wildcard_arg = if inv.more.discard_spare_arguments {
        Some(quote::quote! { #[rest] (String), })
    } else {
        None
    };

    let param_specs =
        inv.parameters
            .iter()
            .map(|p| {
                enum Modifier {
                    None,
                    Lazy,
                    Flag,
                    Rest,
                }

                let modifier =
                    match (p.more.lazy, p.more.rest, p.more.flag) {
                        (false, false, false) => Modifier::None,
                        (true, false, false) => Modifier::Lazy,
                        (false, true, false) => Modifier::Rest,
                        (false, false, true) => Modifier::Flag,
                        _ => return Err(syn::Error::new(
                            p.span,
                            "modifiers like #[lazy] or #[rest] currently cannot be used together",
                        )
                        .into()),
                    };

                let type_ = &p.type_;
                Ok(match modifier {
                    Modifier::Flag => {
                        if p.type_ != syn::parse_quote! { bool } {
                            return Err(
                                syn::Error::new(p.type_.span(), "Must use bool for flags").into()
                            );
                        }
                        let literal = proc_macro2::Literal::string(&p.name.to_string());
                        quote::quote! { #[flag] (#literal) }
                    }
                    Modifier::Lazy => quote::quote! { #[lazy] (#type_) },
                    Modifier::Rest => quote::quote! { #[rest] (#type_) },
                    Modifier::None => quote::quote! { (#type_) },
                })
            })
            .collect::<Result<Vec<_>, Error>>()?;

    // Currently you can either fallback to framework setting or opt-in with zero delay. Rest of the
    // cases are not covered because I don't know how the syntax should look like
    let broadcast_typing = match inv.more.broadcast_typing {
        Some(()) => quote::quote! {
            Some(::poise::BroadcastTypingBehavior::WithDelay(std::time::Duration::from_secs(0)))
        },
        None => quote::quote! {
            None
        },
    };

    let command_name = &inv.command_name;
    let track_edits = inv.more.track_edits;
    let aliases = &inv.more.aliases.0;
    let hide_in_help = &inv.more.hide_in_help;
    let param_names = inv.parameters.iter().map(|p| &p.name).collect::<Vec<_>>();
    let required_permissions = inv.required_permissions;
    let owners_only = inv.more.owners_only;
    Ok(quote::quote! {
        ::poise::PrefixCommand {
            name: #command_name,
            action: |ctx, args| Box::pin(async move {
                let ( #( #param_names, )* .. ) = ::poise::parse_prefix_args!(
                    ctx.discord, ctx.msg, args =>
                    #( #param_specs, )*
                    #wildcard_arg
                ).await?;
                inner(#maybe_wrapped_ctx, #( #param_names, )* ).await
            }),
            options: ::poise::PrefixCommandOptions {
                track_edits: #track_edits,
                broadcast_typing: #broadcast_typing,
                aliases: &[ #( #aliases, )* ],
                inline_help: #description,
                multiline_help: #explanation,
                check: #check,
                on_error: #on_error,
                hide_in_help: #hide_in_help,
                required_permissions: #required_permissions,
                owners_only: #owners_only,
            }
        }
    })
}

fn generate_slash_command_spec(inv: &Invocation) -> Result<proc_macro2::TokenStream, Error> {
    let command_name = &inv.command_name;
    let description = inv.description.as_ref().ok_or_else(|| {
        syn::Error::new(
            inv.function.sig.span(),
            "slash commands must have a description (doc comment)",
        )
    })?;

    let mut parameter_builders = Vec::new();
    for param in inv.parameters {
        let description = param.more.description.as_ref().ok_or_else(|| {
            syn::Error::new(
                param.span,
                "slash command parameters must have a description",
            )
        })?;

        let (mut required, type_) =
            match extract_option_type(&param.type_).or_else(|| extract_vec_type(&param.type_)) {
                Some(t) => (false, t),
                None => (true, &param.type_),
            };

        // Don't require user to input a value for flags - use false as default value (see below)
        if param.more.flag {
            required = false;
        }

        let param_name = &param.name;
        parameter_builders.push((
            quote::quote! {
                |o, framework| (&&&&&std::marker::PhantomData::<#type_>).create(o, framework)
                    .required(#required)
                    .name(stringify!(#param_name))
                    .description(#description)
            },
            required,
        ));
    }
    // Sort the parameters so that optional parameters come last - Discord requires this order
    parameter_builders.sort_by_key(|(_, required)| !required);
    let parameter_builders = parameter_builders
        .into_iter()
        .map(|(builder, _)| builder)
        .collect::<Vec<_>>();

    // Box::pin the check and on_error callbacks in order to store them in a struct
    let check = match &inv.more.check {
        Some(check) => quote::quote! { Some(|ctx| Box::pin(#check(::poise::Context::Slash(ctx)))) },
        None => quote::quote! { None },
    };
    let on_error = match &inv.more.on_error {
        Some(on_error) => quote::quote! {
            Some(|err, ctx| Box::pin(#on_error(err, ::poise::CommandErrorContext::Slash(ctx))))
        },
        None => quote::quote! { None },
    };

    let param_names = inv.parameters.iter().map(|p| &p.name).collect::<Vec<_>>();
    let param_types = inv
        .parameters
        .iter()
        .map(|p| match p.more.flag {
            true => syn::parse_quote! { FLAG },
            false => p.type_.clone(),
        })
        .collect::<Vec<_>>();
    let defer_response = wrap_option(inv.more.defer_response);
    let ephemeral = inv.more.ephemeral;
    let required_permissions = inv.required_permissions;
    let owners_only = inv.more.owners_only;
    Ok(quote::quote! {
        ::poise::SlashCommand {
            name: #command_name,
            description: #description,
            action: |ctx, args| Box::pin(async move {
                // idk why this can't be put in the macro itself (where the lint is triggered) and
                // why clippy doesn't turn off this lint inside macros in the first place
                #[allow(clippy::needless_question_mark)]

                let ( #( #param_names, )* ) = ::poise::parse_slash_args!(
                    ctx.discord, ctx.interaction.guild_id, ctx.interaction.channel_id, ctx.framework, args =>
                    #( (#param_names: #param_types), )*
                ).await?;

                inner(::poise::Context::Slash(ctx), #( #param_names, )*).await
            }),
            parameters: {
                use ::poise::SlashArgumentHack;
                vec![ #( #parameter_builders, )* ]
            },
            options: ::poise::SlashCommandOptions {
                defer_response: #defer_response,
                check: #check,
                on_error: #on_error,
                ephemeral: #ephemeral,
                required_permissions: #required_permissions,
                owners_only: #owners_only,
            }
        }
    })
}

fn command_inner(args: CommandAttrArgs, mut function: syn::ItemFn) -> Result<TokenStream, Error> {
    // Verify that the function is marked async. Not strictly needed, but avoids confusion
    if function.sig.asyncness.is_none() {
        return Err(syn::Error::new(function.sig.span(), "command function must be async").into());
    }

    // Collect argument names/types/attributes to insert into generated function
    let mut parameters = Vec::new();
    for command_param in function.sig.inputs.iter_mut().skip(1) {
        let pattern = match command_param {
            syn::FnArg::Typed(x) => &mut *x,
            syn::FnArg::Receiver(r) => {
                return Err(syn::Error::new(r.span(), "self argument is invalid here").into());
            }
        };
        let name = match &*pattern.pat {
            syn::Pat::Ident(pat_ident) => &pat_ident.ident,
            x => {
                return Err(syn::Error::new(x.span(), "must use an identifier pattern here").into())
            }
        };

        let attrs = pattern
            .attrs
            .drain(..)
            .map(|attr| attr.parse_meta().map(syn::NestedMeta::Meta))
            .collect::<Result<Vec<_>, _>>()?;
        let attrs = <ParamAttrArgs as darling::FromMeta>::from_list(&attrs)?;

        parameters.push(CommandParameter {
            name: name.clone(),
            type_: (*pattern.ty).clone(),
            more: attrs,
            span: command_param.span(),
        });
    }

    let ctx_type = match function.sig.inputs.first() {
        Some(syn::FnArg::Typed(syn::PatType { ty, .. })) => &**ty,
        _ => {
            return Err(syn::Error::new(function.sig.span(), "expected a Context parameter").into())
        }
    };
    let unit_type = syn::parse_quote! { () };
    let return_type = match &function.sig.output {
        syn::ReturnType::Default => &unit_type,
        syn::ReturnType::Type(_, ty) => &*ty,
    };

    // Extract the command descriptionss from the function doc comments
    let (description, explanation) = extract_help_from_doc_comments(&function.attrs);

    let required_permissions = match &args.required_permissions {
        Some(perms) => syn::parse_quote! { poise::serenity_prelude::Permissions::#perms },
        None => syn::parse_quote! { poise::serenity_prelude::Permissions::empty() },
    };

    let invocation = Invocation {
        command_name: args
            .rename
            .clone()
            .unwrap_or_else(|| function.sig.ident.to_string()),
        ctx_type,
        return_type,
        parameters: &parameters,
        description: description.as_deref(),
        explanation: explanation.as_deref(),
        more: &args,
        function: &function,
        required_permissions: &required_permissions,
    };
    let command_spec = generate_prefix_command_spec(&invocation)?;
    let slash_command_spec = wrap_option(if args.slash_command {
        Some(generate_slash_command_spec(&invocation)?)
    } else {
        None
    });

    // Needed because we're not allowed to have lifetimes in the hacky use case below
    let ctx_type_with_static = syn::fold::fold_type(&mut AllLifetimesToStatic, ctx_type.clone());

    let function_name = std::mem::replace(&mut function.sig.ident, syn::parse_quote! { inner });
    let function_visibility = &function.vis;
    Ok(TokenStream::from(quote::quote! {
        #function_visibility fn #function_name() -> (
            ::poise::PrefixCommand<
                <#ctx_type_with_static as poise::_GetGenerics>::U,
                <#ctx_type_with_static as poise::_GetGenerics>::E,
            >,
            Option<::poise::SlashCommand<
                <#ctx_type_with_static as poise::_GetGenerics>::U,
                <#ctx_type_with_static as poise::_GetGenerics>::E,
            >>,
        ) {
            #function

            use ::poise::serenity_prelude as serenity;
            (#command_spec, #slash_command_spec)
        }
    }))
}

#[proc_macro_attribute]
pub fn command(args: TokenStream, function: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(args as Vec<syn::NestedMeta>);
    let args = match <CommandAttrArgs as darling::FromMeta>::from_list(&args) {
        Ok(x) => x,
        Err(e) => return e.write_errors().into(),
    };

    let function = syn::parse_macro_input!(function as syn::ItemFn);

    match command_inner(args, function) {
        Ok(x) => x,
        Err(e) => e.write_errors().into(),
    }
}

#[proc_macro_derive(SlashChoiceParameter, attributes(name))]
pub fn slash_choice_parameter(input: TokenStream) -> TokenStream {
    let enum_ = syn::parse_macro_input!(input as syn::DeriveInput);

    match slash_choice_parameter::slash_choice_parameter(enum_) {
        Ok(x) => x,
        Err(e) => e.write_errors().into(),
    }
}
