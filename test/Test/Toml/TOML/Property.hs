module Test.Toml.TOML.Property where

import Test.Toml.Gen (PropertyTest, genToml)
import Test.Toml.Property (assocLaw, identityLaw)


----------------------------------------------------------------------------
-- Laws
----------------------------------------------------------------------------

test_TomlAssocLaw :: PropertyTest
test_TomlAssocLaw = assocLaw genToml

test_TomlIdentityLaw :: PropertyTest
test_TomlIdentityLaw = identityLaw genToml
