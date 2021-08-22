//! Parse received slash command arguments into Rust types.

use std::convert::{TryFrom, TryInto};
use std::marker::PhantomData;

use crate::serenity_prelude as serenity;

#[derive(Debug)]
pub enum SlashArgError {
    CommandStructureMismatch(&'static str),
    Parse(Box<dyn std::error::Error + Send + Sync>),
    IntegerOutOfBounds,
}
impl std::fmt::Display for SlashArgError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CommandStructureMismatch(detail) => {
                write!(
                    f,
                    "Bot author did not register their commands correctly ({})",
                    detail
                )
            }
            Self::Parse(e) => write!(f, "Failed to parse argument: {}", e),
            Self::IntegerOutOfBounds => write!(f, "Integer out of bounds for target type"),
        }
    }
}
impl std::error::Error for SlashArgError {
    fn cause(&self) -> Option<&dyn std::error::Error> {
        match self {
            Self::Parse(e) => Some(&**e),
            Self::CommandStructureMismatch(_) => None,
            Self::IntegerOutOfBounds => None,
        }
    }
}

/// Implement this trait on types that you want to use as a slash command parameter.
#[async_trait::async_trait]
pub trait SlashArgument<'f, U, E>: Sized {
    /// Extract a Rust value of type T from the slash command argument, given via a
    /// [`serde_json::Value`].
    async fn extract(
        ctx: &serenity::Context,
        guild: Option<serenity::GuildId>,
        channel: Option<serenity::ChannelId>,
        framework: &'f crate::Framework<U, E>,
        value: &serde_json::Value,
    ) -> Result<Self, SlashArgError>;

    /// Create a slash command parameter equivalent to type T.
    ///
    /// Only fields about the argument type are filled in. The caller is still responsible for
    /// filling in `name()`, `description()`, and possibly `required()` or other fields.
    fn create<'a>(
        builder: &'a mut serenity::CreateApplicationCommandOption,
        framework: &'f crate::Framework<U, E>,
    ) -> &'a mut serenity::CreateApplicationCommandOption;
}

/// Implemented for all types that can be used as a function parameter in a slash command.
///
/// Currently marked `#[doc(hidden)]` because implementing this trait requires some jank due to a
/// `PhantomData` hack and the auto-deref specialization hack.
#[doc(hidden)]
#[async_trait::async_trait]
pub trait SlashArgumentHack<'f, T, U, E> {
    async fn extract(
        self,
        ctx: &serenity::Context,
        guild: Option<serenity::GuildId>,
        channel: Option<serenity::ChannelId>,
        framework: &'f crate::Framework<U, E>,
        value: &serde_json::Value,
    ) -> Result<T, SlashArgError>;

    fn create<'a>(
        self,
        builder: &'a mut serenity::CreateApplicationCommandOption,
        framework: &'f crate::Framework<U, E>,
    ) -> &'a mut serenity::CreateApplicationCommandOption;
}

/// Handles arbitrary types that can be parsed from string.
#[async_trait::async_trait]
impl<'f, T, U: Send + Sync, E> SlashArgumentHack<'f, T, U, E> for PhantomData<T>
where
    T: serenity::ArgumentConvert + Send + Sync,
    T::Err: std::error::Error + Send + Sync + 'static,
{
    async fn extract(
        self,
        ctx: &serenity::Context,
        guild: Option<serenity::GuildId>,
        channel: Option<serenity::ChannelId>,
        _: &'f crate::Framework<U, E>,
        value: &serde_json::Value,
    ) -> Result<T, SlashArgError> {
        let string = value
            .as_str()
            .ok_or(SlashArgError::CommandStructureMismatch("expected string"))?;
        T::convert(ctx, guild, channel, string)
            .await
            .map_err(|e| SlashArgError::Parse(e.into()))
    }

    fn create<'a>(
        self,
        builder: &'a mut serenity::CreateApplicationCommandOption,
        _: &'f crate::Framework<U, E>,
    ) -> &'a mut serenity::CreateApplicationCommandOption {
        builder.kind(serenity::ApplicationCommandOptionType::String)
    }
}

// Handles all integers, signed and unsigned, via TryFrom<i64>.
#[async_trait::async_trait]
impl<'f, T: TryFrom<i64> + Send + Sync, U: Send + Sync, E> SlashArgumentHack<'f, T, U, E>
    for &PhantomData<T>
{
    async fn extract(
        self,
        _: &serenity::Context,
        _: Option<serenity::GuildId>,
        _: Option<serenity::ChannelId>,
        _: &'f crate::Framework<U, E>,
        value: &serde_json::Value,
    ) -> Result<T, SlashArgError> {
        value
            .as_i64()
            .ok_or(SlashArgError::CommandStructureMismatch("expected integer"))?
            .try_into()
            .ok()
            .ok_or(SlashArgError::IntegerOutOfBounds)
    }

    fn create<'a>(
        self,
        builder: &'a mut serenity::CreateApplicationCommandOption,
        _: &'f crate::Framework<U, E>,
    ) -> &'a mut serenity::CreateApplicationCommandOption {
        builder.kind(serenity::ApplicationCommandOptionType::Integer)
    }
}

#[async_trait::async_trait]
impl<'f, U: Send + Sync, E> SlashArgumentHack<'f, f32, U, E> for &&PhantomData<f32> {
    async fn extract(
        self,
        _: &serenity::Context,
        _: Option<serenity::GuildId>,
        _: Option<serenity::ChannelId>,
        _: &'f crate::Framework<U, E>,
        value: &serde_json::Value,
    ) -> Result<f32, SlashArgError> {
        Ok(value
            .as_f64()
            .ok_or(SlashArgError::CommandStructureMismatch("expected float"))? as f32)
    }

    fn create<'a>(
        self,
        builder: &'a mut serenity::CreateApplicationCommandOption,
        _: &'f crate::Framework<U, E>,
    ) -> &'a mut serenity::CreateApplicationCommandOption {
        builder.kind(serenity::ApplicationCommandOptionType::Number)
    }
}

#[async_trait::async_trait]
impl<'f, U: Send + Sync, E> SlashArgumentHack<'f, f64, U, E> for &&PhantomData<f64> {
    async fn extract(
        self,
        _: &serenity::Context,
        _: Option<serenity::GuildId>,
        _: Option<serenity::ChannelId>,
        _: &'f crate::Framework<U, E>,
        value: &serde_json::Value,
    ) -> Result<f64, SlashArgError> {
        value
            .as_f64()
            .ok_or(SlashArgError::CommandStructureMismatch("expected float"))
    }

    fn create<'a>(
        self,
        builder: &'a mut serenity::CreateApplicationCommandOption,
        _: &'f crate::Framework<U, E>,
    ) -> &'a mut serenity::CreateApplicationCommandOption {
        builder.kind(serenity::ApplicationCommandOptionType::Number)
    }
}

#[async_trait::async_trait]
impl<'f, T: SlashArgument<'f, U, E> + Sync, U: Send + Sync, E> SlashArgumentHack<'f, T, U, E>
    for &&&PhantomData<T>
{
    async fn extract(
        self,
        ctx: &serenity::Context,
        guild: Option<serenity::GuildId>,
        channel: Option<serenity::ChannelId>,
        framework: &'f crate::Framework<U, E>,
        value: &serde_json::Value,
    ) -> Result<T, SlashArgError> {
        <T as SlashArgument<'f, U, E>>::extract(ctx, guild, channel, framework, value).await
    }

    fn create<'a>(
        self,
        builder: &'a mut serenity::CreateApplicationCommandOption,
        framework: &'f crate::Framework<U, E>,
    ) -> &'a mut serenity::CreateApplicationCommandOption {
        <T as SlashArgument<'f, U, E>>::create(builder, framework)
    }
}

// Implement slash argument for a model type that is represented in interactions via an ID
macro_rules! impl_slash_argument {
    ($type:ty, $slash_param_type:ident) => {
        #[async_trait::async_trait]
        impl<'f, U: Send + Sync, E> SlashArgumentHack<'f, $type, U, E> for &&PhantomData<$type> {
            async fn extract(
                self,
                ctx: &serenity::Context,
                guild: Option<serenity::GuildId>,
                channel: Option<serenity::ChannelId>,
                framework: &'f crate::Framework<U, E>,
                value: &serde_json::Value,
            ) -> Result<$type, SlashArgError> {
                // We can parse IDs by falling back to the generic serenity::ArgumentConvert impl
                PhantomData::<$type>
                    .extract(ctx, guild, channel, framework, value)
                    .await
            }

            fn create<'a>(
                self,
                builder: &'a mut serenity::CreateApplicationCommandOption,
                _: &'f crate::Framework<U, E>,
            ) -> &'a mut serenity::CreateApplicationCommandOption {
                builder.kind(serenity::ApplicationCommandOptionType::$slash_param_type)
            }
        }
    };
}
impl_slash_argument!(serenity::Member, User);
impl_slash_argument!(serenity::User, User);
impl_slash_argument!(serenity::Channel, Channel);
impl_slash_argument!(serenity::GuildChannel, Channel);
impl_slash_argument!(serenity::Role, Role);

#[doc(hidden)]
#[macro_export]
macro_rules! _parse_slash {
    // Extract Option<T>
    ($ctx:ident, $guild_id:ident, $channel_id:ident, $framework:ident, $args:ident => $name:ident: Option<$type:ty $(,)*>) => {
        #[allow(clippy::eval_order_dependence)]
        if let Some(arg) = $args.iter().find(|arg| arg.name == stringify!($name)) {
            let arg = arg.value
            .as_ref()
            .ok_or($crate::SlashArgError::CommandStructureMismatch("expected argument value"))?;
            Some(
                (&&&&&std::marker::PhantomData::<$type>)
                .extract($ctx, $guild_id, Some($channel_id), $framework, arg)
                .await?
            )
        } else {
            None
        }
    };

    // Extract Vec<T> (delegating to Option<T> because slash commands don't support variadic
    // arguments right now)
    ($ctx:ident, $guild_id:ident, $channel_id:ident, $framework:ident, $args:ident => $name:ident: Vec<$type:ty $(,)*>) => {
        match $crate::_parse_slash!($ctx, $guild_id, $channel_id, $framework, $args => $name: Option<$type>) {
            Some(value) => vec![value],
            None => vec![],
        }
    };

    // Extract #[flag]
    ($ctx:ident, $guild_id:ident, $channel_id:ident, $framework:ident, $args:ident => $name:ident: FLAG) => {
        $crate::_parse_slash!($ctx, $guild_id, $channel_id, $framework, $args => $name: Option<bool>)
            .unwrap_or(false)
    };

    // Extract T
    ($ctx:ident, $guild_id:ident, $channel_id:ident, $framework:ident, $args:ident => $name:ident: $($type:tt)*) => {
        $crate::_parse_slash!($ctx, $guild_id, $channel_id, $framework, $args => $name: Option<$($type)*>)
            .ok_or($crate::SlashArgError::CommandStructureMismatch("a required argument is missing"))?
    };
}

#[macro_export]
macro_rules! parse_slash_args {
    ($ctx:expr, $guild_id:expr, $channel_id:expr, $framework:expr, $args:expr => $(
        ( $name:ident: $($type:tt)* )
    ),* $(,)? ) => {
        async /* not move! */ {
            use $crate::SlashArgumentHack;

            let (ctx, guild_id, channel_id, framework, args) = ($ctx, $guild_id, $channel_id, $framework, $args);

            Ok::<_, $crate::SlashArgError>(( $(
                $crate::_parse_slash!( ctx, guild_id, channel_id, framework, args => $name: $($type)* ),
            )* ))
        }
    };
}
