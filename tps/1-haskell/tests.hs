import Test.HUnit

-- Your imports and function definitions

-- Sample Personajes and Objetos for testing
thor = Personaje (0, 0) "Thor"
mjölnir = Objeto (2, 2) "Mjölnir"

-- Test cases for foldPersonaje
testsFoldPersonaje = [
  "foldPersonaje test1" ~: foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) thor ~?= 0,
  "foldPersonaje test2" ~: foldPersonaje (\p s -> 0) (\r d -> r+1) (\r -> r+1) (Mueve thor Norte) ~?= 1,
  "foldPersonaje test3" ~: foldPersonaje (\p s -> s) (\r d -> r) (\r -> r) (Muere thor) ~?= "Thor"
  ]

-- Test cases for foldObjeto
testsFoldObjeto = [
  "foldObjeto test1" ~: foldObjeto (\p s -> (0, 0)) (\r p -> posición_personaje p) (\r -> (1, 1)) mjölnir ~?= (2, 2),
  "foldObjeto test2" ~: foldObjeto (\p s -> s) (\r p -> nombre_personaje p) (\r -> "Objeto destruido") mjölnir ~?= "Mjölnir",
  "foldObjeto test3" ~: foldObjeto (\p s -> p) (\r p -> p) (\r -> p) (Tomado mjölnir thor) ~?= thor
  ]

-- Test cases for posición_personaje
testsPosiciónPersonaje = [
  "posición_personaje test1" ~: posición_personaje thor ~?= (0,0),
  "posición_personaje test2" ~: posición_personaje (Mueve thor Este) ~?= (1,0)
  ]

-- Test cases for nombre_objeto
testsNombreObjeto = [
  "nombre_objeto test1" ~: nombre_objeto mjölnir ~?= "Mjölnir",
  "nombre_objeto test2" ~: nombre_objeto (Tomado mjölnir thor) ~?= "Mjölnir",
  "nombre_objeto test3" ~: nombre_objeto (EsDestruido mjölnir) ~?= "Mjölnir"
  ]

-- Test cases for objetos_en
testsObjetosEn = [
  "objetos_en test1" ~: objetos_en [Right mjölnir, Left thor] ~?= [mjölnir],
  "objetos_en test2" ~: objetos_en [Left thor, Right mjölnir, Left thor, Right mjölnir] ~?= [mjölnir, mjölnir]
  ]

-- Test cases for objetos_en_posesión_de
testsObjetosEnPosesiónDe = [
  "objetos_en_posesión_de test1" ~: objetos_en_posesión_de thor [Right mjölnir, Right mjölnir] ~?= [mjölnir, mjölnir]
  ]

-- Test cases for objeto_libre_mas_cercano
testsObjetoLibreMasCercano = [
  "objeto_libre_mas_cercano test1" ~: objeto_libre_mas_cercano thor [Right mjölnir, Right mjölnir] ~?= mjölnir
  ]

-- Test cases for tiene_thanos_todas_las_gemas
testsTieneThanosTodasLasGemas = [
  "tiene_thanos_todas_las_gemas test1" ~: tiene_thanos_todas_las_gemas [Right mjölnir, Right mjölnir] ~?= False,
  "tiene_thanos_todas_las_gemas test2" ~: tiene_thanos_todas_las_gemas [Right gemaMente, Right gemaRealidad, Right gemaEspacio, Right gemaPoder, Right gemaAlma, Right gemaTiempo] ~?= True
  ]

-- Test cases for podemos_ganarle_a_thanos
testsPodemosGanarleAThanos = [
  "podemos_ganarle_a_thanos test1" ~: podemos_ganarle_a_thanos [Left thor, Right mjölnir] ~?= True,
  "podemos_ganarle_a_thanos test2" ~: podemos_ganarle_a_thanos [Left thor] ~?= False
  ]

-- Combine all tests
allTests = "allTests" ~: test [
  "foldPersonaje" ~: testsFoldPersonaje,
  "foldObjeto" ~: testsFoldObjeto,
  "posición_personaje" ~: testsPosiciónPersonaje,
  "nombre_objeto" ~: testsNombreObjeto,
  "objetos_en" ~: testsObjetosEn,
  "objetos_en_posesión_de" ~: testsObjetosEnPosesiónDe,
  "objeto_libre_mas_cercano" ~: testsObjetoLibreMasCercano,
  "tiene_thanos_todas_las_gemas" ~: testsTieneThanosTodasLasGemas,
  "podemos_ganarle_a_thanos" ~: testsPodemosGanarleAThanos
  ]

-- Run the tests
main :: IO Counts
main = do runTestTT allTests