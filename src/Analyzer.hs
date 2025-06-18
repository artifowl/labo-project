{-# LANGUAGE LambdaCase #-}

module Analyzer (analyzeBufferOverflow, analyzeUninitializedVars, formatBufferOverflowRisks, formatUninitWarnings) where

import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Syntax.Constants
import Data.List (nub, find, intersect, union)
import Data.Maybe (isJust)



{-
__________        _____  _____              ________                      _____.__                 
\______   \__ ___/ ____\/ ____\___________  \_____  \___  __ ____________/ ____\  |   ______  _  __
 |    |  _/  |  \   __\\   __\/ __ \_  __ \  /   |   \  \/ // __ \_  __ \   __\|  |  /  _ \ \/ \/ /
 |    |   \  |  /|  |   |  | \  ___/|  | \/ /    |    \   /\  ___/|  | \/|  |  |  |_(  <_> )     / 
 |______  /____/ |__|   |__|  \___  >__|    \_______  /\_/  \___  >__|   |__|  |____/\____/ \/\_/  
        \/                        \/                \/          \/                                 

-}


-- | Liste des fonctions dangereuses susceptibles de causer des buffer overflows
dangerousFunctions :: [String]
dangerousFunctions = ["strcpy", "strcat", "gets", "sprintf"]

-- | Liste des fonctions d'allocation dynamique
allocFunctions :: [String]
allocFunctions = ["malloc", "calloc", "realloc"]

-- | Représente un buffer (nom, taille éventuelle si tableau connu)
type BufferInfo = (String, Maybe Int)

-- | Analyse principale : parcourt toutes les déclarations externes
analyzeBufferOverflow :: [CExtDecl] -> [(String, Position)]
analyzeBufferOverflow = concatMap analyzeExtDecl

-- | Analyse une déclaration externe
analyzeExtDecl :: CExtDecl -> [(String, Position)]
analyzeExtDecl (CFDefExt (CFunDef _ _ _ (CCompound _ stmts _) _)) =
    let buffers = nub $ concatMap collectBuffers stmts
        allocated = nub $ concatMap collectAllocatedBuffers stmts
        -- On considère comme "safe" les buffers alloués dynamiquement
        safeNames = allocated
        riskyBuffers = filter (\(name, _) -> name `notElem` safeNames) buffers
    in concatMap (analyzeStmt riskyBuffers) stmts
analyzeExtDecl _ = []

-- | Collecte les buffers déclarés (nom + taille si connue)
collectBuffers :: CBlockItem -> [BufferInfo]
collectBuffers = \case
    CBlockDecl (CDecl _ declrs _) -> concatMap declNameIfBuffer declrs
    _ -> []

-- | Extrait les informations de buffer d'une déclaration
declNameIfBuffer :: (Maybe (CDeclarator NodeInfo), Maybe (CInitializer NodeInfo), Maybe (CExpression NodeInfo)) -> [BufferInfo]
declNameIfBuffer (Just (CDeclr (Just (Ident name _ _)) derived _ _ _), _, _) =
    case find isBufferDecl derived of
        Just (CArrDeclr _ (CArrSize _ (CConst (CIntConst i _))) _) ->
            [(name, Just (fromIntegral $ getCInteger' i))]
        Just _ ->
            [(name, Nothing)]
        _ -> []
  where
    isBufferDecl (CArrDeclr _ _ _) = True
    isBufferDecl (CPtrDeclr _ _)   = True
    isBufferDecl _                 = False
    getCInteger' (CInteger v _ _) = v
declNameIfBuffer _ = []



-- | Collecte les buffers alloués dynamiquement
collectAllocatedBuffers :: CBlockItem -> [String]
collectAllocatedBuffers = \case
    CBlockStmt (CExpr (Just (CAssign _ (CVar (Ident name _ _) _) rhs _)) _) ->
        case unwrapCall rhs of
            Just fname | fname `elem` allocFunctions -> [name]
            _ -> []
    CBlockStmt (CCompound _ stmts _) -> concatMap collectAllocatedBuffers stmts
    _ -> []

-- | Enlève les cast pour trouver les appels d'allocation
unwrapCall :: CExpr -> Maybe String
unwrapCall = \case
    CCall (CVar (Ident fname _ _) _) _ _ -> Just fname
    CCast _ e _ -> unwrapCall e
    _ -> Nothing

-- | Analyse un bloc
analyzeStmt :: [BufferInfo] -> CBlockItem -> [(String, Position)]
analyzeStmt buffers = \case
    CBlockStmt stmt -> analyzeStatement buffers stmt
    CBlockDecl _    -> []
    _               -> []

-- | Analyse une instruction
analyzeStatement :: [BufferInfo] -> CStat -> [(String, Position)]
analyzeStatement buffers = \case
    CExpr (Just expr) _ -> analyzeExpr buffers expr
    CCompound _ stmts _ -> concatMap (analyzeStmt buffers) stmts
    CIf _ thenSt (Just elseSt) _ -> analyzeStatement buffers thenSt ++ analyzeStatement buffers elseSt
    CIf _ thenSt Nothing _ -> analyzeStatement buffers thenSt
    CWhile _ body _ _ -> analyzeStatement buffers body
    CFor _ _ _ body _ -> analyzeStatement buffers body
    _ -> []

-- | Analyse une expression
analyzeExpr :: [BufferInfo] -> CExpr -> [(String, Position)]
analyzeExpr buffers = \case
    CCall (CVar (Ident fname _ _) _) args ni
        | fname `elem` dangerousFunctions ->
            let argNames = concatMap extractVarNames args
                bufferNames = map fst buffers
                dangerousUses = filter (`elem` bufferNames) argNames
                pos = posOfNode ni
            in [("Call to dangerous function '" ++ fname ++ "' with buffer '" ++ n ++ "' at " ++ show pos, pos) | n <- dangerousUses]

    CIndex (CVar (Ident arrName _ _) _) (CConst (CIntConst i _)) ni
        | Just limit <- lookup arrName buffers >>= id,
          let index = fromIntegral $ getCInteger' i,
          index >= limit ->
            let pos = posOfNode ni
            in [("Buffer overflow: '" ++ arrName ++ "[" ++ show index ++ "]' exceeds size " ++ show limit ++ " at " ++ show pos, pos)]
    _ -> []
  where
    getCInteger' (CInteger v _ _) = v

-- | Extraction simple de noms de variables dans une expression
extractVarNames :: CExpr -> [String]
extractVarNames = \case
    CVar (Ident name _ _) _ -> [name]
    _ -> []

-- | Affichage des alertes
printBufferOverflowRisks :: [(String, Position)] -> IO ()
printBufferOverflowRisks [] = putStrLn "✅ No risk of buffer overflow detected."
printBufferOverflowRisks risks =
    mapM_ (\(msg, _) -> putStrLn $ "⚠️  Warning: " ++ msg) risks

formatBufferOverflowRisks :: [(String, Position)] -> String
formatBufferOverflowRisks [] = "<span class='success'>✅ No risk of buffer overflow detected.</span>"
formatBufferOverflowRisks risks =
    unlines $ map (\(msg, _) -> "<span class='warning'>⚠️  Warning: " ++ msg ++ "</span>") risks




{-

            .__       .__  __  .__       .__  .__                  .___                     .__      ___.   .__                 
 __ __  ____ |__| ____ |__|/  |_|__|____  |  | |__|_______ ____   __| _/ ___  _______ _______|__|____ \_ |__ |  |   ____   ______
|  |  \/    \|  |/    \|  \   __\  \__  \ |  | |  \___   // __ \ / __ |  \  \/ /\__  \\_  __ \  \__  \ | __ \|  | _/ __ \ /  ___/
|  |  /   |  \  |   |  \  ||  | |  |/ __ \|  |_|  |/    /\  ___// /_/ |   \   /  / __ \|  | \/  |/ __ \| \_\ \  |_\  ___/ \___ \ 
|____/|___|  /__|___|  /__||__| |__(____  /____/__/_____ \\___  >____ |    \_/  (____  /__|  |__(____  /___  /____/\___  >____  >
           \/        \/                 \/              \/    \/     \/              \/              \/    \/          \/     \/ 

-}

-- | Analyse des variables non initialisées utilisées
analyzeUninitializedVars :: [CExtDecl] -> [(String, Position)]
analyzeUninitializedVars = concatMap analyzeUninitExt

analyzeUninitExt :: CExtDecl -> [(String, Position)]
analyzeUninitExt (CFDefExt (CFunDef _ _ _ (CCompound _ stmts _) _)) =
    snd $ analyzeUninitStmts [] [] stmts
analyzeUninitExt _ = []

-- | Analyse des blocs pour les variables non initialisées
-- Retourne (variables initialisées, liste des warnings)
analyzeUninitStmts :: [String] -> [String] -> [CBlockItem] -> ([String], [(String, Position)])
analyzeUninitStmts initialized declared = go initialized declared
  where
    go inits _ [] = (inits, [])

    go inits declared (item:rest) = case item of

        -- Déclaration de variables 
        CBlockDecl (CDecl _ decls _) ->
            let
                (updatedInits, updatedDeclared, warnings) =
                    foldl (\(accInits, accDeclared, accWarns) decl ->
                        let (i, d, w) = processDecl decl (accInits, accDeclared)
                        in (i, d, accWarns ++ w)
                    ) (inits, declared, []) decls

                (restInits, restWarns) = go updatedInits updatedDeclared rest
            in (restInits, warnings ++ restWarns)
        -- Expression avec affectation potentielle
        CBlockStmt (CExpr (Just expr) ni) ->
            let (inits', warnings) = analyzeExpr' expr inits declared ni
                (restInits, restWarns) = go inits' declared rest
            in (restInits, warnings ++ restWarns)

        -- Bloc composé (ex: { ... })
        CBlockStmt (CCompound _ innerStmts _) ->
            let (_, innerWarns) = analyzeUninitStmts inits declared innerStmts
                (restInits, restWarns) = go inits declared rest
            in (restInits, innerWarns ++ restWarns)

        -- Gestion du if
        CBlockStmt (CIf cond thenBlock elseBlock ni) ->
            let
                (_, condWarnings) = analyzeExpr' cond inits declared ni

                (thenInits, thenWarnings) = analyzeUninitStmts inits declared (extractBlockStmtsFromStmt thenBlock)

                (elseInits, elseWarnings) = maybe (inits, []) (analyzeUninitStmts inits declared . extractBlockStmtsFromStmt) elseBlock

                mergedInits = intersect thenInits elseInits

                (restInits, restWarnings) = go mergedInits declared rest
            in (restInits, condWarnings ++ thenWarnings ++ elseWarnings ++ restWarnings)

        -- Gestion du for
        CBlockStmt (CFor init cond update body ni) ->
            let
                -- Étape 1 : extraire déclarations locales au for
                (initInits, localDeclared, initWarnings) = case init of
                    Right (CDecl _ decls _) ->
                        let
                            -- Récupérer les noms déclarés
                            names = [ name
                                    | (Just (CDeclr (Just (Ident name _ _)) _ _ _ _), _, _) <- decls ]

                            -- Les analyser comme d’habitude
                            (i', _, w') = foldl
                                (\(i, d, w) decl ->
                                    let (i2, d2, w2) = processDecl decl (i, d)
                                    in (i2, d2, w ++ w2)
                                ) ([], [], []) decls
                        in (i', names, w')
                    Left maybeExpr -> case maybeExpr of
                        Just expr -> let (i', w') = analyzeExpr' expr inits declared ni in (i', [], w')
                        Nothing   -> (inits, [], [])

                -- Étape 2 : construire environnement déclaré localement au for
                declaredInFor = localDeclared ++ declared

                -- Étape 3 : analyser condition, update et corps
                (condInits, condWarnings) = case cond of
                    Just expr -> analyzeExpr' expr initInits declaredInFor ni
                    Nothing   -> (initInits, [])

                (updateInits, updateWarnings) = case update of
                    Just expr -> analyzeExpr' expr condInits declaredInFor ni
                    Nothing   -> (condInits, [])

                (bodyInits, bodyWarnings) = analyzeUninitStmts updateInits declaredInFor (extractBlockStmtsFromStmt body)

                -- ⚠️ Étape 4 : calcul de l’état après la boucle
                -- ⚠️ NE PAS propager les variables déclarées dans le for
                initsAfterFor = inits `union` initInits `union` condInits `union` updateInits
                -- bodyInits pas inclus car la boucle peut ne pas s’exécuter

                (restInits, restWarnings) = go initsAfterFor declared rest

            in (restInits, initWarnings ++ condWarnings ++ updateWarnings ++ bodyWarnings ++ restWarnings)

        -- Autres cas ignorés
        _ -> go inits declared rest

    -- Analyse expressions avec détection d'initialisation et warnings
    analyzeExpr' :: CExpr -> [String] -> [String] -> NodeInfo -> ([String], [(String, Position)])
    analyzeExpr' (CAssign _ (CVar (Ident name _ _) _) _ _) inits declared _ =
        let newInits = if name `elem` declared && name `notElem` inits then name : inits else inits
        in (newInits, [])

    analyzeExpr' (CCall (CVar (Ident funcName _ _) _) args ni) inits declared _ =
        let
            initVars =
                if funcName == "scanf"
                then [ name
                    | CUnary CAdrOp (CVar (Ident name _ _) _) _ <- args
                    , name `elem` declared
                    ]
                else case args of
                    (CVar (Ident name _ _) _ : _)
                        | funcName `elem` ["strcpy", "strncpy", "sprintf", "snprintf", "gets", "memcpy", "memmove", "strcat", "strncat"]
                        -> [name | name `elem` declared]
                    _ -> []

            newInits = inits ++ filter (`elem` declared) initVars
            usedVars = concatMap extractVarNames' args
            uninitUsed = filter (`elem` declared) $ filter (`notElem` newInits) usedVars
            pos = posOfNode ni
            warnings = [("May use of uninitialized variable '" ++ v ++ "' at " ++ show pos, pos) | v <- uninitUsed]
        in (newInits, warnings)

    analyzeExpr' expr inits declared ni =
        let used = extractVarNames' expr
            undeclared = filter (`notElem` declared) used
            uninitUsed = filter (`elem` declared) $ filter (`notElem` inits) used
            pos = posOfNode ni
            undeclaredWarnings = [("Use of undeclared variable '" ++ v ++ "' at " ++ show pos, pos) | v <- undeclared]
            uninitWarnings = [("Use of uninitialized variable '" ++ v ++ "' at " ++ show pos, pos) | v <- uninitUsed]
        in (inits, undeclaredWarnings ++ uninitWarnings)

-- | Traitement des déclarations avec détection dans l'initialisation
processDecl :: (Maybe CDeclr, Maybe CInit, Maybe CExpr)
            -> ([String], [String])  -- (initialized, declared)
            -> ([String], [String], [(String, Position)])
processDecl (Just (CDeclr (Just (Ident name _ _)) _ _ _ _), maybeInit, _) (accInits, accDeclared) =
    let declaredNow = name : accDeclared
    in case maybeInit of
        Nothing ->
            (accInits, declaredNow, [])

        Just (CInitExpr expr _) ->
            let usedVars = extractVarNames' expr
                uninitUsed = filter (`elem` declaredNow) $ filter (`notElem` accInits) usedVars
                pos = posOfNode (nodeInfo expr)
                warnings = [("Use of uninitialized variable '" ++ v ++ "' in initialization of '" ++ name ++ "' at " ++ show pos, pos)
                            | v <- uninitUsed]
            in (name : accInits, declaredNow, warnings)

        _ -> (name : accInits, declaredNow, [])
processDecl _ acc = (fst acc, snd acc, [])
-- | Extraction des noms de variables utilisés dans une expression
extractVarNames' :: CExpr -> [String]
extractVarNames' (CVar (Ident name _ _) _) = [name]
extractVarNames' (CIndex e1 e2 _) = extractVarNames' e1 ++ extractVarNames' e2
extractVarNames' (CCall e args _) = extractVarNames' e ++ concatMap extractVarNames' args
extractVarNames' (CAssign _ e1 e2 _) = extractVarNames' e1 ++ extractVarNames' e2
extractVarNames' (CCast _ e _) = extractVarNames' e
extractVarNames' (CUnary _ e _) = extractVarNames' e
extractVarNames' (CBinary _ e1 e2 _) = extractVarNames' e1 ++ extractVarNames' e2
extractVarNames' _ = []

-- | Extract statements from a block or singleton statement
extractBlockStmtsFromStmt :: CStatement NodeInfo -> [CBlockItem]
extractBlockStmtsFromStmt (CCompound _ stmts _) = stmts
extractBlockStmtsFromStmt stmt = [CBlockStmt stmt]

-- | Affichage des avertissements
printUninitWarnings :: [(String, Position)] -> IO ()
printUninitWarnings [] = putStrLn "✅ No uninitialized variables detected."
printUninitWarnings warns =
    mapM_ (\(msg, _) -> putStrLn $ "⚠️  Warning: " ++ msg) warns


formatUninitWarnings :: [(String, Position)] -> String
formatUninitWarnings [] = "<span class='success'><br>✅ No use of uninitialized variables detected.</span>"
formatUninitWarnings warnings =
    unlines $ map (\(msg, _) -> "<span class='warning'>⚠️  Warning: " ++ msg ++ "</span>") warnings