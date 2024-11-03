use syn::spanned::Spanned as _;

use crate::util::wrap_option;

use super::Invocation;

fn generate_context_menu_action(inv: &Invocation) -> Result<proc_macro2::TokenStream, syn::Error> {
    let param_type = match &*inv.parameters {
        [single_param] => &single_param.type_,
        _ => {
            return Err(syn::Error::new(
                inv.function.sig.inputs.span(),
                "Context menu commands require exactly one parameter",
            ))
        }
    };

    Ok(quote::quote! {
        <#param_type as ::poise::ContextMenuParameter<_, _>>::to_action(|ctx, value| {
            Box::pin(async move {
                if !ctx.framework.options.manual_cooldowns {
                    ctx.command.cooldowns.lock().unwrap().start_cooldown(ctx.cooldown_context());
                }

                inner(ctx.into(), value)
                    .await
                    .map_err(|error| poise::FrameworkError::new_command(
                        ctx.into(),
                        error,
                    ))
            })
        })
    })
}

pub fn generate_context_menu_command(inv: &Invocation) -> syn::Result<proc_macro2::TokenStream> {
    let action = generate_context_menu_action(inv)?;
    let name = wrap_option(inv.args.context_menu_command.as_deref());

    Ok(quote::quote!(
        poise::ContextMenuCommand {
            action: #action,
            name: #name,
        }
    ))
}
