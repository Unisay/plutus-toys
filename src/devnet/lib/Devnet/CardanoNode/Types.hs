module Devnet.CardanoNode.Types (
  RunningNode (..),
) where

import Cardano.Api (
  Env,
  LocalNodeConnectInfo,
  NetworkId,
 )

-- | Describes a running pool node
data RunningNode = RunningNode
  { rnNodeSocket :: FilePath
  -- ^ Cardano node socket
  , rnNetworkId :: NetworkId
  -- ^ Network ID used by the cardano node
  , rnNodeConfigFile :: FilePath
  -- ^ Cardano node config file (JSON)
  , rnConnectInfo :: (LocalNodeConnectInfo, Env)
  -- ^ Connection info for node queries
  }
