-- MemberTest.hs
module MemberTest where

import Test.HUnit
import Data.Member

-- Prueba de getNewMemberInfo
testGetNewMemberInfo :: Test
testGetNewMemberInfo = TestCase $ do
  let memberInfo = Member 0 "John" "john@example.com" []
  newMember <- runTestIO getNewMemberInfo
  assertEqual "getNewMemberInfo" memberInfo newMember

-- Prueba de updateMember
testUpdateMember :: Test
testUpdateMember = TestCase $ do
  let members = [Member 1 "Alice" "alice@example.com" [], Member 2 "Bob" "bob@example.com" []]
  let updatedMember = Member 1 "Alice Updated" "alice.updated@example.com" []
  let expectedMembers = [updatedMember, Member 2 "Bob" "bob@example.com" []]

  let result = updateMember 1 updatedMember members
  assertEqual "updateMember" expectedMembers result

-- Puedes agregar más pruebas según tus necesidades

-- Ejecutar las pruebas
main :: IO ()
main = do
  runTestTT $ TestList
    [ testGetNewMemberInfo
    , testUpdateMember
    -- Agrega más pruebas aquí
    ]
  return ()
