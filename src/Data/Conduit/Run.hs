{-|
Module: Data.Conduit.Run

General convenience functions and error handling for running executables on
a Linux system and capturing the text they output.

NOTE: Because it uses 'sourceProcessWithStreams' from "Data.Conduit.Process",
this module requires a threaded runtime to function properly.
-}

{-# Language KindSignatures #-}
{-# Language RankNTypes #-}

module Data.Conduit.Run
      -- * Running processes
    ( runTransparent
    , runSemiTransparent
    , runOpaque
      -- ** Low level
    , tryProcess
    , transDump
      -- * Defining output
    , OutputType
      -- ** Output sinks #outputsinks#
    , textOutput
    , lenientTextOutput
    , dataOutput
    , linesOutput
      -- ** Output types
    , TextOutput
    , DataOutput
    , LinesOutput
      -- ** Output stream tags
    , StdOut(..)
    , StdErr(..)
      -- * Handling errors
    , RunProcessException(..)
    , fatal
      -- * Re-exports
    , Typeable
    ) where

import Conduit
import Control.Exception.Safe
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString as BS
import Data.Conduit.Process
import Data.Kind
import GHC.Stack
import System.Exit
import System.IO
import qualified Data.Text.Lazy as Lazy

-- | Run a process while transparently dumping @stdout@ and @stderr@ data
--   streams to their respective handles, capturing the output for later use.
--   Throws a fatal error in the event of an exception.
--
--   Completely ignores @stdin@. Any @Ctrl+C@ events are passed to the running
--   process using 'delegate_ctlc'.
runTransparent
    :: (Typeable out, Show out)
    -- | What type of output should be produced (use an option from
    --   [Output sinks](#g:outputsinks))
    => OutputType out
    -- | Path of the executable
    -> FilePath
    -- | Arguments to pass to the executable
    -> [String]
    -> IO (StdOut out, StdErr out)
runTransparent (OutputSink otr) exe args = fatal $ do
    let cp = (proc exe args)
            { delegate_ctlc = True }
    tryProcess Nothing (transDump stdout .| otr) (transDump stderr .| otr) cp
        >>= either throwIO pure

-- | Run a process while transparently dumping the @stderr@ data
--   stream to its respective handle, capturing the output for later use.
--   Throws a fatal error in the event of an exception.
--
--   Completely ignores @stdin@. Any @Ctrl+C@ events are passed to the running
--   process using 'delegate_ctlc'.
runSemiTransparent
    :: (Typeable out, Show out)
    -- | What type of output should be produced (use an option from
    --   [Output sinks](#g:outputsinks))
    => OutputType out
    -- | Path of the executable
    -> FilePath
    -- | Arguments to pass to the executable
    -> [String]
    -> IO (StdOut out, StdErr out)
runSemiTransparent (OutputSink otr) exe args = fatal $ do
    let cp = (proc exe args) 
            { delegate_ctlc = True }
    tryProcess Nothing otr (transDump stderr .| otr) cp
        >>= either throwIO pure

-- | Run a process, capturing the output for later use.
--   Throws a fatal error in the event of an exception.
--
--   Completely ignores @stdin@.
runOpaque
    :: (Typeable out, Show out)
    -- | What type of output should be produced (use an option from
    --   [Output sinks](#g:outputsinks))
    => OutputType out
    -- | Path of the executable
    -> FilePath
    -- | Arguments to pass to the executable
    -> [String]
    -> IO (StdOut out, StdErr out)
runOpaque (OutputSink otr) exe args = fatal $ do
    let cp = proc exe args
    tryProcess Nothing otr otr cp
        >>= either throwIO pure

-- | Run a process and either return the @stdout@ and @stderr@ streams,
--   or a 'RunProcessException' in the event of an error.
--
--   Completely ignores @stdin@.
tryProcess
    :: forall out.
       -- | An optional conduit producer for generating data to be passed to
       --   @stdin@
       Maybe (ConduitT () ByteString IO ())
       -- | A conduit/sink for manipulating @stdout@ and writing to @out@
       --  (using e.g. 'sinkLazy')
    -> ConduitT ByteString Void IO out
       -- | A conduit/sink for manipulating @stderr@ and writing to @out@
       --  (using e.g. 'sinkLazy')
    -> ConduitT ByteString Void IO out
       -- | Process spec to run. You can use @'proc' exe args@ as a default.
       --   Note that 'std_in', 'std_out', and 'std_err' are subject to be
       --   overridden internally within this function.
    -> CreateProcess
    -> IO (Either (RunProcessException out) (StdOut out, StdErr out))
tryProcess mInC outC errC cp0 = do
    let (inPipe, inC) = case mInC of
            Just c -> (CreatePipe, c)
            Nothing  -> (NoStream, pure ())
        cp = cp0
                { std_in = inPipe
                , std_out = CreatePipe
                , std_err = CreatePipe }

    r <- tryAny $ sourceProcessWithStreams cp inC outC errC
    pure $ case r of
        Left e -> Left $ RPEException cp e
        Right (ec, o, e) -> case ec of
            ExitFailure _ -> Left $ RPEFailure cp ec o e
            ExitSuccess -> Right (StdOut o, StdErr e)

-- | > newtype OutputType out
--   >     = OutputSink (forall m. MonadThrow m => ConduitT ByteString Void m out)
newtype OutputType out
    = OutputSink (forall m. MonadThrow m => ConduitT ByteString Void m out)

-- | Output in the form of lazy text
type TextOutput = OutputType Lazy.Text

-- | Output in the form of a lazy bytestring
type DataOutput = OutputType LazyByteString

-- | Output in the form of a list of strict bytestrings, one per line of input
type LinesOutput = OutputType [ByteString]

-- | Convert output to 'Lazy.Text' using 'decodeUtf8C'
textOutput :: TextOutput
textOutput = OutputSink $ decodeUtf8C .| sinkLazy

-- | Convert output to 'Lazy.Text' using 'decodeUtf8LenientC'
lenientTextOutput :: TextOutput
lenientTextOutput = OutputSink $ decodeUtf8LenientC .| sinkLazy

-- | Pass the output through unchanged, resulting in a 'LazyByteString'
dataOutput :: DataOutput
dataOutput = OutputSink sinkLazy

-- | Split into lines using 'linesUnboundedAsciiC', outputting as a list of
--   strict bytestrings, one per line of input
linesOutput :: LinesOutput
linesOutput = OutputSink $ linesUnboundedAsciiC .| sinkList

-- | A captured stream from @stdout@
newtype StdOut (out :: Type) = StdOut { unStdOut :: out }

-- | A captured stream from @stderr@
newtype StdErr (out :: Type) = StdErr { unStdErr :: out }

-- | A process either returned an 'ExitFailure' or an exception
data RunProcessException out
    = RPEFailure
        { rpeCreateProcess :: CreateProcess
        , rpeExitCode :: ExitCode
        , rpeStdout :: out
        , rpeStderr :: out
        }
    | RPEException
        { rpeCreateProcess :: CreateProcess
        , rpeException :: SomeException
        }
    deriving Show

instance (Typeable out, Show out) => Exception (RunProcessException out) where
    displayException (RPEFailure cp ec out er) = unlines $
        [ "Error when running \"" ++ showCmdSpec cp ++ "\""
        , "Exit code: " ++ show ec
        , "stdout: " ++ show out
        , "stderr: " ++ show er
        ]
    displayException (RPEException cp e) = unlines $
        [ "Error when running \"" ++ showCmdSpec cp ++ "\""
        , displayException e
        ]

-- | When any sort of exception occurs, treat it as fatal: print the
--   exception's message and call stack, then terminate.
fatal :: HasCallStack => IO a -> IO a
fatal act = withFrozenCallStack $ catchAny act $ \e -> die $ unlines
    $ "A fatal error occurred:"
    : ( ("    " ++) -- indent each line by four spaces
          <$> lines (displayException e)
          ++  [ "" ]
          ++  lines (prettyCallStack callStack)
      )

-- | Transparently dump the incoming stream to the specified handle, passing
--   the stream along unchanged
transDump :: Handle -> ConduitT ByteString ByteString IO ()
transDump = iterMC . BS.hPut

-- | Convert the 'cmdspec' to a 'String'
showCmdSpec :: CreateProcess -> String
showCmdSpec cp = case cmdspec cp of
    ShellCommand s -> s
    RawCommand fp ss -> unwords $ fp : ss
