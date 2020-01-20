module Main where

import           Data.IntMap.Strict as IntMap
import           Data.IntSet        as IntSet
import           Data.IORef
import           Data.Map.Strict    as Map
import           Data.Set           as Set
import           Expression
import           Lexer              (alexScanTokens)
import           Parser             (parseLambda)
import           System.IO          (isEOF)

getString :: IO String
getString = do
    endMark <- isEOF
    if endMark then return ""
    else do
         current <- getLine
         tail <- getString
         return $! current ++ " " ++ tail


readInts :: IO [Int]
readInts = fmap (Prelude.map read . words) getLine


collectFree :: Expression -> Set String -> IO (Set String)
collectFree expr set = do
    --e <- prettyShow expr
    --putStrLn e
    case expr of
      Var s -> --return $! Set.singleton s
        case Set.member s set of
            True  -> return $! Set.empty
            False -> return $! Set.singleton s
      Apply a b -> do
          as <- collectFree a set
          bs <- collectFree b set
          return $! Set.union as bs
      Ref ref -> do
          exp <- readIORef ref
          collectFree exp set
      Lambda s e -> collectFree e $! Set.insert s set

replaceFreeOnArgs :: Int -> String -> IORef Expression -> Set String -> Map String String -> Expression -> Bool -> IO Expression
replaceFreeOnArgs round to_replace reference set repls expr replace_needed =
    let contRep = replaceFreeOnArgs round to_replace reference set repls
    in case expr of
        Var s | s == to_replace && replace_needed -> return (Ref reference)
              | otherwise ->
                  case Map.lookup s repls of
                    Nothing   -> return expr
                    Just repl -> return (Var repl)
        Apply a b -> do
            ae <- contRep a replace_needed
            be <- contRep b replace_needed
            return $! (Apply ae be)
        Lambda s e | s == to_replace -> do
                        ee <- contRep e False
                        return $! (Lambda s ee)
                   | Set.member s set -> do
                        let name = s ++ (show round)
                        ee <- (replaceFreeOnArgs round to_replace reference set $! (Map.insert s name repls)) e replace_needed
                        return $! (Lambda name ee)
                   | otherwise -> do
                        ee <- contRep e replace_needed
                        return $! (Lambda s ee)
        Ref ref -> do
           e <- readIORef ref
           contRep e replace_needed


    {--
replaceFreeOnArgs :: Int -> Set String -> Map String String -> Expression -> IO Expression
replaceFreeOnArgs round set repls expr =
    let contRep = replaceFreeOnArgs round set repls
    in case expr of
        Var s -> case Map.lookup s repls of
                    Nothing   -> return expr
                    Just repl -> return (Var repl)
        Apply a b -> do
            ae <- contRep a
            be <- contRep b
            return (Apply ae be)
        Lambda s e -> case Set.member s set of
                        True -> do
                            let name = s ++ (show round)
                            ee <- replaceFreeOnArgs round set (Map.insert s name repls) e
                            return (Lambda name ee)
                        False -> do
                            ee <- contRep e
                            return (Lambda s ee)
        Ref ref -> do
           e <- readIORef ref
           replaceFreeOnArgs round set repls e

replaceWithRef :: String -> IORef Expression -> Expression -> Expression
replaceWithRef name ref expr =
    let cont = replaceWithRef name ref
    in case expr of
        Var s | s == name -> Ref ref
              | otherwise -> expr
        Apply a b -> Apply (cont a) (cont b)
        Lambda s e | s == name -> expr
                   | otherwise -> Lambda s (cont e)
        Ref i -> error "All refs should be already broken here"
--}

doMagic :: Int -> Expression -> Expression -> String -> IO (Expression, Bool)
doMagic i p q s = do
    -- pp <- prettyShow p
    -- pq <- prettyShow q
    -- putStrLn $ "I substitute in " ++ pp ++ " variable " ++ s ++ " with " ++ pq
    reference <- newIORef q
    free <- collectFree q Set.empty
    expr' <- replaceFreeOnArgs i s reference free Map.empty p True
    --expr' <- replaceFreeOnArgs i free Map.empty p
    --let expr'' = replaceWithRef s reference expr'
    return $! (expr', True)


makeBasicApply i p q = do
    (pexpr, reduced) <- betaReduce i p
    if reduced
        then return $! (Apply pexpr q, True)
        else do
            (qexpr, qreduced) <- betaReduce i q
            return $! (Apply p qexpr, qreduced)

absoluteRead :: IORef Expression -> IO Expression
absoluteRead ref = do
    exp <- readIORef ref
    case exp of
      Ref ref' -> id $! absoluteRead ref'
      _        -> return $! exp


betaReduce :: Int -> Expression -> IO (Expression, Bool)
betaReduce round expr = do
    -- putStrLn "Searching reduction"
    -- peexpr <- prettyShow expr
    -- putStrLn $ peexpr
    case expr of
        Var _                -> return $! (expr, False)
        Lambda s p           -> do
            (pexpr, reduced) <- betaReduce round p
            return $! (Lambda s pexpr, reduced)
        -- Apply (Ref i) q -> return $ doMagic i map
        Apply (Lambda s p) q -> doMagic round p q s
        Apply p@(Ref ref) q      -> do
            exp <- absoluteRead ref
            case exp of
              Lambda s e -> doMagic round e q s
              _          -> makeBasicApply round p q
        Apply p q -> makeBasicApply round p q
        Ref ref -> do
            e <- readIORef ref
            (ne, reduced) <- betaReduce round e
            writeIORef ref ne
            return $! (expr, reduced)


prettyShow :: Expression -> IO String
prettyShow expr =
    case expr of
      Var s -> return s
      Apply a b -> do
            pa <- prettyShow a
            pb <- prettyShow b
            return $ "(" ++ pa ++ " " ++ pb ++ ")"
      Lambda s e -> do
            pe <- prettyShow e
            return $ "(\\" ++ s ++ "." ++ pe ++ ")"
      Ref ref -> do
        exp <- readIORef ref
        prettyShow exp
        -- pexp <- prettyShow exp
        --return $! "{" ++ pexp ++ "}"


recursiveCycle :: Int -> Int -> Int -> (Int -> Expression -> IO (Expression, Bool)) -> Expression -> IO ()
recursiveCycle i to when what expr = do
    --putStrLn $ "==========="
    if (i == to)
        then return ()
        else do
            (nexpr, reduced) <- what i expr
            -- putStrLn $ show nmap
            if not reduced
               then if (mod (i - 1) when == 0)
                       then return ()
                       else do
                            printable <- prettyShow nexpr
                            putStrLn $ printable
               else do
                   if (mod i when == 0)
                        --then putStrLn $ show nexpr
                        then do
                            printable <- prettyShow nexpr
                            putStrLn $ printable
                        else putStr ""
                   -- let nnmap = clearUseless nmap nexpr
                   recursiveCycle (i + 1) to when what nexpr



main :: IO ()
main = do
    [m, k] <- readInts
    --putStrLn $ show a
    line <- getString
    case parseLambda $ alexScanTokens line of
      Left err   -> putStrLn err
      Right expr -> do
          e <-  prettyShow expr
          -- putStrLn "========="
          putStrLn e
          id $! recursiveCycle 1 (m + 1) k (betaReduce) expr
