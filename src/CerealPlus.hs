module CerealPlus 
  (
    SerializableM(..),
    serialize,
    deserialize,
    runSerializeT,
    runDeserializeT,
  ) where

import CerealPlus.Prelude
import qualified CerealPlus.SerializeT as SerializeT; import CerealPlus.SerializeT (Serialize, SerializeT)
import qualified CerealPlus.DeserializeT as DeserializeT; import CerealPlus.DeserializeT (Deserialize, DeserializeT)
import CerealPlus.SerializableM


serialize :: (SerializableM a Identity) => a -> Serialize ()
serialize = serializeT

deserialize :: (SerializableM a Identity) => Deserialize a
deserialize = deserializeT

runSerializeT = SerializeT.run
runDeserializeT = DeserializeT.run


-- TODO

-- runSerialize = runIdentity . SerializeT.run

-- runDeserialize deserialize = runIdentity . DeserializeT.run deserialize





