module Lib where
import Text.Show.Functions

laVerdad = True

type Calorias = Float
type Minutos = Float
type Kilos = Float
type Ejercicio = Minutos -> Gimnasta -> Gimnasta

data Gimnasta = Gimnasta {
    nombre :: String,
    edad :: Float,
    peso :: Float,
    tonificacion :: Float 
} deriving(Show)

pancho = Gimnasta "Francisco" 40 120 1
andres = Gimnasta "Andy" 22 80 6

estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = not (esObeso gimnasta)   && tonificacion gimnasta > 5 
 
esObeso :: Gimnasta -> Bool
esObeso = (>100) . peso

quemarCalorias :: Gimnasta -> Calorias -> Gimnasta
quemarCalorias gimnasta calorias
                | esObeso gimnasta = bajarPeso gimnasta (calorias / 150)
                | not(esObeso gimnasta) && edad gimnasta > 30 && calorias > 200 = bajarPeso gimnasta 1
                | otherwise = bajarPeso gimnasta (calorias / ((peso gimnasta) * (edad gimnasta)))

bajarPeso :: Gimnasta -> Kilos -> Gimnasta
bajarPeso gimnasta kilos = gimnasta { peso = (peso gimnasta) - kilos}       

caminataEnCinta :: Ejercicio 
caminataEnCinta minutos gimnasta = quemarCalorias gimnasta (1 * 5 * minutos)

entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta minutos gimnasta = quemarCalorias gimnasta (calcularCalorias minutos)

calcularCalorias :: Minutos -> Calorias
calcularCalorias minutos = 1 * ( (6 + velocidadMaxima minutos) / 2 ) * minutos

velocidadMaxima :: Minutos -> Float
velocidadMaxima = (+6).(/5) 

pesas :: Kilos -> Ejercicio
pesas kilos minutos gimnasta = tonificar (calcularTonificacion kilos minutos) gimnasta

calcularTonificacion :: Kilos -> Minutos -> Float
calcularTonificacion kilos minutos 
                        | minutos > 10 = kilos / 10
                        | otherwise = 0

tonificar :: Float -> Gimnasta -> Gimnasta 
tonificar porcentajeTonificacion gimnasta = gimnasta { tonificacion = tonificacion gimnasta + porcentajeTonificacion }

colina :: Float -> Ejercicio
colina inclinacion minutos gimnasta = quemarCalorias gimnasta (2 * minutos * inclinacion)

montania :: Float -> Ejercicio
montania inclinacion minutos  = tonificar 1 . colina (inclinacion + 3) (minutos / 2) . colina inclinacion (minutos / 2)

relax :: Ejercicio
relax minutos gimnasta = gimnasta

data Rutina = Rutina {
    nombreRutina :: String,
    duracion :: Float,
    ejercicios :: [Ejercicio]
}

localizada = Rutina "Localizada" 40 [caminataEnCinta, entrenamientoEnCinta, pesas 50, colina 5, montania 5]

funcional = Rutina "Funcional" 40 [ pesas 50]

abdominales = Rutina "Abdominales" 40 [ pesas 50, entrenamientoEnCinta]

realizarRutina :: Gimnasta -> Rutina -> Gimnasta
realizarRutina gimnasta rutina = foldl (hacerEjercicio (duracion rutina)) gimnasta (ejercicios rutina)

hacerEjercicio :: Minutos -> Gimnasta -> Ejercicio -> Gimnasta
hacerEjercicio minutos gimnasta ejercicio = ejercicio minutos gimnasta

realizarRutinaRecursivo :: Rutina -> Gimnasta -> Gimnasta
realizarRutinaRecursivo rutina gimnasta = realizarEjerciciosRutina (ejercicios rutina) gimnasta (duracion rutina)

realizarEjerciciosRutina :: [Ejercicio] -> Gimnasta -> Minutos -> Gimnasta
realizarEjerciciosRutina [] gimnasta _ = gimnasta
realizarEjerciciosRutina (cabeza:cola) gimnasta minutos = realizarEjerciciosRutina cola (cabeza minutos gimnasta) minutos

resumenRutina :: Gimnasta -> Rutina -> (String, Kilos, Float)
resumenRutina gimnasta rutina = ( nombreRutina rutina, calcularDiferencia peso rutina gimnasta , calcularDiferencia tonificacion rutina gimnasta )

calcularDiferencia :: (Gimnasta -> Float) -> Rutina -> Gimnasta -> Float
calcularDiferencia f rutina gimnasta =  f gimnasta - f (realizarRutina gimnasta rutina) 

rutinas = [localizada, funcional, abdominales]

rutinasSaludable :: Gimnasta -> [Rutina] -> [(String, Kilos, Float)]
rutinasSaludable gimnasta =  map (resumenRutina gimnasta) . filter (esSaludableRutina gimnasta)

esSaludableRutina :: Gimnasta -> Rutina -> Bool
esSaludableRutina gimnasta = estaSaludable . realizarRutina gimnasta 