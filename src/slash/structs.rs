//! Holds slash-command definition structs.

use crate::{serenity_prelude as serenity, BoxFuture, Framework};

#[non_exhaustive]
pub struct SlashContext<'a, U, E> {
    pub discord: &'a serenity::Context,
    pub interaction: &'a serenity::ApplicationCommandInteraction,
    pub has_sent_initial_response: &'a std::sync::atomic::AtomicBool,
    pub framework: &'a Framework<U, E>,
    pub command: &'a SlashCommand<U, E>,
    pub data: &'a U,
}
impl<U, E> Clone for SlashContext<'_, U, E> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<U, E> Copy for SlashContext<'_, U, E> {}
impl<U, E> crate::_GetGenerics for SlashContext<'_, U, E> {
    type U = U;
    type E = E;
}

impl<U, E> SlashContext<'_, U, E> {
    pub async fn defer_response(&self) -> Result<(), serenity::Error> {
        self.interaction
            .create_interaction_response(self.discord, |f| {
                f.kind(serenity::InteractionResponseType::DeferredChannelMessageWithSource)
            })
            .await?;
        self.has_sent_initial_response
            .store(true, std::sync::atomic::Ordering::SeqCst);
        Ok(())
    }
}

pub struct SlashCommandErrorContext<'a, U, E> {
    pub while_checking: bool,
    pub command: &'a SlashCommand<U, E>,
    pub ctx: SlashContext<'a, U, E>,
}

impl<U, E> Clone for SlashCommandErrorContext<'_, U, E> {
    fn clone(&self) -> Self {
        Self {
            while_checking: self.while_checking,
            command: self.command,
            ctx: self.ctx,
        }
    }
}

/// Configuration for a singular slash command or a slash subcommand group
pub struct SlashCommandOptions<U, E> {
    /// Falls back to the framework-specified value on None. See there for documentation.
    pub on_error: Option<fn(E, SlashCommandErrorContext<'_, U, E>) -> BoxFuture<'_, ()>>,
    /// If this function returns false, this command will not be executed.
    pub check: Option<fn(SlashContext<'_, U, E>) -> BoxFuture<'_, Result<bool, E>>>,
    /// Falls back to the framework-specified value on None. See there for documentation.
    pub defer_response: Option<bool>,
    /// Whether responses to this command should be ephemeral by default.
    pub ephemeral: bool,
    /// Permissions which a user needs to have so that the slash command runs.
    pub required_permissions: serenity::Permissions,
}

impl<U, E> Default for SlashCommandOptions<U, E> {
    fn default() -> Self {
        Self {
            on_error: None,
            check: None,
            defer_response: None,
            ephemeral: false,
            required_permissions: serenity::Permissions::empty(),
        }
    }
}

/// A single slash command with an associated action, parameters, and configuration.
///
/// Subcommands are not stored here, see [`SlashCommandMeta`] instead.
pub struct SlashCommand<U, E> {
    pub name: &'static str,
    pub description: &'static str,
    pub action: for<'a> fn(
        SlashContext<'a, U, E>,
        &'a [serenity::ApplicationCommandInteractionDataOption],
    ) -> BoxFuture<'a, Result<(), E>>,
    pub parameters: Vec<
        // TODO: change to just serenity::CreateApplicationCommandOption?
        fn(
            &mut serenity::CreateApplicationCommandOption,
        ) -> &mut serenity::CreateApplicationCommandOption,
    >,
    pub options: SlashCommandOptions<U, E>,
}

pub enum SlashCommandMeta<U, E> {
    Command(SlashCommand<U, E>),
    SubcommandGroup {
        name: &'static str,
        description: &'static str,
        subcommands: Vec<SlashCommandMeta<U, E>>,
        options: SlashCommandOptions<U, E>,
    },
}

impl<U, E> SlashCommandMeta<U, E> {
    pub async fn create_in_guild(
        &self,
        http: &serenity::Http,
        guild_id: serenity::GuildId,
    ) -> Result<serenity::ApplicationCommand, serenity::Error> {
        guild_id
            .create_application_command(http, |c| self.create(c))
            .await
    }

    pub async fn create_global(
        &self,
        http: &serenity::Http,
    ) -> Result<serenity::ApplicationCommand, serenity::Error> {
        serenity::ApplicationCommand::create_global_application_command(http, |c| self.create(c))
            .await
    }

    pub fn create_as_subcommand<'a>(
        &self,
        builder: &'a mut serenity::CreateApplicationCommandOption,
    ) -> &'a mut serenity::CreateApplicationCommandOption {
        match self {
            Self::SubcommandGroup {
                name,
                description,
                subcommands,
            } => {
                builder.kind(serenity::ApplicationCommandOptionType::SubCommandGroup);
                builder.name(name).description(description);

                for sub_subcommand in subcommands {
                    builder.create_sub_option(|f| sub_subcommand.create_as_subcommand(f));
                }
            }
            Self::Command(command) => {
                builder.kind(serenity::ApplicationCommandOptionType::SubCommand);
                builder.name(command.name).description(command.description);

                for create_option in &command.parameters {
                    let mut option = serenity::CreateApplicationCommandOption::default();
                    create_option(&mut option);
                    builder.add_sub_option(option);
                }
            }
        }
        builder
    }

    pub fn create<'a>(
        &self,
        interaction: &'a mut serenity::CreateApplicationCommand,
    ) -> &'a mut serenity::CreateApplicationCommand {
        match self {
            Self::SubcommandGroup {
                name,
                description,
                subcommands,
            } => {
                interaction.name(name).description(description);

                for subcommand in subcommands {
                    interaction.create_option(|f| subcommand.create_as_subcommand(f));
                }
            }
            Self::Command(command) => {
                interaction
                    .name(command.name)
                    .description(command.description);

                for create_option in &command.parameters {
                    let mut option = serenity::CreateApplicationCommandOption::default();
                    create_option(&mut option);
                    interaction.add_option(option);
                }
            }
        }
        interaction
    }
}

pub struct SlashFrameworkOptions<U, E> {
    /// List of bot commands.
    pub commands: Vec<SlashCommand<U, E>>,
    /// Provide a callback to be invoked before every command. The command will only be executed
    /// if the callback returns true.
    ///
    /// Individual commands may override this callback.
    pub command_check: fn(SlashContext<'_, U, E>) -> BoxFuture<'_, Result<bool, E>>,
    /// Whether to send an interaction acknoweldgement.
    ///
    /// Individual commands may override this value.
    ///
    /// If true, send back a quick interaction acknowledgement when receiving an interaction, which
    /// gives your code 15 minutes to respond. When this field is set to false, no acknowledgement
    /// is sent and you need to respond within 3 seconds or the interaction is considered failed by
    /// Discord.
    ///
    /// In some way this is the equivalent of `crate::PrefixFrameworkOptions::broadcast_typing`.
    pub defer_response: bool,
    /// Invoked when a user tries to execute a slash command but doesn't have the required
    /// permissions for it.
    ///
    /// This handler should be used to reply with some form of error message. If this handler does
    /// nothing, the user will be shown "Interaction failed" by their Discord client.
    pub missing_permissions_handler: fn(SlashContext<'_, U, E>) -> BoxFuture<'_, ()>,
}

impl<U: Send + Sync, E> Default for SlashFrameworkOptions<U, E> {
    fn default() -> Self {
        Self {
            commands: Vec::new(),
            command_check: |_| Box::pin(async { Ok(true) }),
            defer_response: false,
            missing_permissions_handler: |ctx| {
                Box::pin(async move {
                    let response = format!(
                        "You don't have the required permissions for `/{}`",
                        ctx.command.name
                    );
                    let _: Result<_, _> =
                        crate::send_slash_reply(ctx, |f| f.content(response).ephemeral(true)).await;
                })
            },
        }
    }
}
