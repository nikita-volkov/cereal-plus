{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import HTFTestSuite.Prelude
import CerealPlus.Serializable
import qualified CerealPlus.Serialize as S
import qualified CerealPlus.Deserialize as D

main = htfMain $ htf_thisModulesTests



test_deserializeBoolOutOfRange = do
  bs <- S.exec $ serialize $ ('a')
  traceM $ show bs
  r :: D.Result IO Bool <- D.runPartial deserialize bs 
  case r of
    D.Fail t bs -> assertEqual ("Out of range", "") (t, bs)
    D.Partial c -> error $ "Partial"
    D.Done r bs -> error $ "Done: " <> show r <> " " <> show bs
