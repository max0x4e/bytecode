module BytecodeSpec where

import SpecHelper

spec :: Spec
spec = do
    describe "Bytecode.runInterpreter" $ do
        context "Stack-based language" $ do
            it "executes statements for a simple stack-based language" $ do
                let code = "# This is just a comment\n\
                            \ 4 5 +"
                runInterpreter code

main :: IO () 
main = hspec spec