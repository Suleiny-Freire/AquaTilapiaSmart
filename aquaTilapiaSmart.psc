// ============================================================================
// PROYECTO 4 - AQUATILAPIA SMART: control de aireacion y recirculacion
// Simulacion en PSeInt (las lecturas se ingresan por teclado)
// ============================================================================

Funcion clasificacion <- clasificarOxigeno(valorOxigeno)
    Definir clasificacion Como Cadena
    Si valorOxigeno < 3 Entonces
        clasificacion <- "CRITICO"
    SiNo
        Si valorOxigeno < 5 Entonces
            clasificacion <- "BAJO"
        SiNo
            Si valorOxigeno < 6 Entonces
                clasificacion <- "ACEPTABLE"
            SiNo
                clasificacion <- "OPTIMO"
            FinSi
        FinSi
    FinSi
FinFuncion

// VERDADERO = aireador encendido. Entre 5 y 6 mg/L se mantiene el estado previo.
Funcion nuevoEstado <- decidirAireador(valorOxigeno, estadoPrevio)
    Definir nuevoEstado Como Logico
    Si valorOxigeno < 5 Entonces
        nuevoEstado <- Verdadero
    SiNo
        Si valorOxigeno >= 6 Entonces
            nuevoEstado <- Falso
        SiNo
            nuevoEstado <- estadoPrevio
        FinSi
    FinSi
FinFuncion

// Prioridad 1: sin agua se bloquea la bomba. Prioridad 2: tierra humeda, pausa.
// Prioridad 3: temporizador ciclico (faseRiego = VERDADERO envia agua).
Funcion bombaEncendida <- decidirBomba(valorDistancia, valorHumedad, faseRiego)
    Definir bombaEncendida Como Logico
    Si valorDistancia > 15 Entonces
        bombaEncendida <- Falso
    SiNo
        Si valorHumedad >= 75 Entonces
            bombaEncendida <- Falso
        SiNo
            bombaEncendida <- faseRiego
        FinSi
    FinSi
FinFuncion

SubProceso mostrarMenu
    Escribir ""
    Escribir "===== CONTROL AQUATILAPIA SMART ====="
    Escribir "1. Registrar mediciones de oxigeno, temperatura, humedad y nivel"
    Escribir "2. Consultar el estado de la piscina"
    Escribir "3. Evaluar y simular el control del aireador y la bomba"
    Escribir "4. Consultar el estado del aireador y la bomba"
    Escribir "5. Consultar el historial de mediciones y activaciones"
    Escribir "6. Mostrar estadisticas y alertas"
    Escribir "7. Finalizar el programa"
FinSubProceso

SubProceso mostrarEstadoPiscina(valorOxigeno, valorTemperatura, valorHumedad, valorDistancia, clasificacion)
    Escribir "--- Estado de la piscina ---"
    Escribir "Oxigeno disuelto: ", valorOxigeno, " mg/L"
    Escribir "Temperatura: ", valorTemperatura, " C"
    Escribir "Humedad del sustrato: ", valorHumedad, " %"
    Escribir "Distancia sensor-agua: ", valorDistancia, " cm"
    Escribir "Clasificacion del oxigeno: ", clasificacion
    Si valorTemperatura < 25 O valorTemperatura > 30 Entonces
        Escribir "[ALERTA TERMICA] Temperatura fuera del rango ideal (25 a 30 C)."
    FinSi
    Si valorDistancia > 15 Entonces
        Escribir "[SEGURIDAD] Nivel de agua bajo (distancia mayor a 15 cm)."
    FinSi
FinSubProceso

SubProceso mostrarEstadoActuadores(estadoAire, estadoBomba, modoManual, activaciones)
    Escribir "--- Estado de los actuadores ---"
    Si modoManual Entonces
        Escribir "Modo de operacion: MANUAL"
    SiNo
        Escribir "Modo de operacion: AUTOMATICO"
    FinSi
    Si estadoAire Entonces
        Escribir "El aireador esta ENCENDIDO."
    SiNo
        Escribir "El aireador esta APAGADO."
    FinSi
    Si estadoBomba Entonces
        Escribir "La bomba esta ENCENDIDA."
    SiNo
        Escribir "La bomba esta APAGADA."
    FinSi
    Escribir "Activaciones del aireador: ", activaciones
FinSubProceso

SubProceso mostrarHistorial(mediciones, estadosAire, estadosBomba, cantidad)
    Definir i Como Entero
    Definir textoAire, textoBomba Como Cadena
    Escribir "--- Historial de mediciones ---"
    Si cantidad = 0 Entonces
        Escribir "Todavia no hay rondas registradas."
    SiNo
        Para i <- 1 Hasta cantidad Hacer
            Si estadosAire[i] Entonces
                textoAire <- "ENCENDIDO"
            SiNo
                textoAire <- "APAGADO"
            FinSi
            Si estadosBomba[i] Entonces
                textoBomba <- "ENCENDIDA"
            SiNo
                textoBomba <- "APAGADA"
            FinSi
            Escribir "Ronda ", i, " | O2: ", mediciones[i,1], " mg/L | Temp: ", mediciones[i,2], " C | Hum: ", mediciones[i,3], " % | Nivel: ", mediciones[i,4], " cm | Aireador: ", textoAire, " | Bomba: ", textoBomba
        FinPara
    FinSi
FinSubProceso

Algoritmo controlAquaTilapia
    Definir opcionMenu, modoOperacion, i, contadorRondas, contadorActivaciones, alertasCriticas, alertasNivel Como Entero
    Definir oxigeno, temperatura, humedad, distAgua Como Real
    Definir sumaOxigeno, sumaTemperatura, promedioOxigeno, promedioTemperatura Como Real
    Definir minOxigeno, maxOxigeno, minTemperatura, maxTemperatura Como Real
    Definir estadoAireador, estadoBomba, nuevoEstadoAireador, modoManual, faseRiego Como Logico
    Definir clasificacion Como Cadena
    Definir historialMediciones Como Real
    Definir historialAireador, historialBomba Como Logico

    // Matriz: fila = ronda; columnas: 1 oxigeno, 2 temperatura, 3 humedad, 4 distancia al agua
    Dimensionar historialMediciones(20,4)
    // Arreglos: estado del aireador y de la bomba en cada ronda (VERDADERO = encendido)
    Dimensionar historialAireador(20)
    Dimensionar historialBomba(20)

    opcionMenu <- 0
    contadorRondas <- 0
    contadorActivaciones <- 0
    estadoAireador <- Falso
    estadoBomba <- Falso
    modoManual <- Falso
    faseRiego <- Verdadero
    oxigeno <- 0
    temperatura <- 0
    humedad <- 0
    distAgua <- 0

    Mientras opcionMenu <> 7 Hacer
        mostrarMenu()
        Escribir "Elija una opcion:"
        Leer opcionMenu

        Segun opcionMenu Hacer
            1:
                Si contadorRondas >= 20 Entonces
                    Escribir "El historial esta lleno (20 rondas). No se pueden registrar mas mediciones."
                SiNo
                    Repetir
                        Escribir "Ingrese el oxigeno disuelto (0 a 20 mg/L):"
                        Leer oxigeno
                        Si oxigeno < 0 O oxigeno > 20 Entonces
                            Escribir "Valor invalido. Debe estar entre 0 y 20."
                        FinSi
                    Hasta Que oxigeno >= 0 Y oxigeno <= 20
                    Repetir
                        Escribir "Ingrese la temperatura (0 a 40 C):"
                        Leer temperatura
                        Si temperatura < 0 O temperatura > 40 Entonces
                            Escribir "Valor invalido. Debe estar entre 0 y 40."
                        FinSi
                    Hasta Que temperatura >= 0 Y temperatura <= 40
                    Repetir
                        Escribir "Ingrese la humedad del sustrato (0 a 100 %):"
                        Leer humedad
                        Si humedad < 0 O humedad > 100 Entonces
                            Escribir "Valor invalido. Debe estar entre 0 y 100."
                        FinSi
                    Hasta Que humedad >= 0 Y humedad <= 100
                    Repetir
                        Escribir "Ingrese la distancia del sensor al agua (0 a 50 cm):"
                        Leer distAgua
                        Si distAgua < 0 O distAgua > 50 Entonces
                            Escribir "Valor invalido. Debe estar entre 0 y 50."
                        FinSi
                    Hasta Que distAgua >= 0 Y distAgua <= 50
                    contadorRondas <- contadorRondas + 1
                    historialMediciones[contadorRondas,1] <- oxigeno
                    historialMediciones[contadorRondas,2] <- temperatura
                    historialMediciones[contadorRondas,3] <- humedad
                    historialMediciones[contadorRondas,4] <- distAgua
                    historialAireador[contadorRondas] <- estadoAireador
                    historialBomba[contadorRondas] <- estadoBomba
                    Escribir "Ronda ", contadorRondas, " registrada correctamente."
                FinSi
            2:
                Si contadorRondas = 0 Entonces
                    Escribir "Primero registre una medicion (opcion 1)."
                SiNo
                    clasificacion <- clasificarOxigeno(oxigeno)
                    mostrarEstadoPiscina(oxigeno, temperatura, humedad, distAgua, clasificacion)
                FinSi
            3:
                Si contadorRondas = 0 Entonces
                    Escribir "Primero registre una medicion (opcion 1)."
                SiNo
                    Repetir
                        Escribir "Modo de operacion (1 = Automatico, 2 = Manual):"
                        Leer modoOperacion
                        Si modoOperacion <> 1 Y modoOperacion <> 2 Entonces
                            Escribir "Opcion invalida. Elija 1 o 2."
                        FinSi
                    Hasta Que modoOperacion = 1 O modoOperacion = 2
                    modoManual <- (modoOperacion = 2)
                    Si modoManual Entonces
                        nuevoEstadoAireador <- Verdadero
                        estadoBomba <- Verdadero
                        Escribir "[INFO] Modo manual: aireador y bomba en flujo continuo."
                    SiNo
                        nuevoEstadoAireador <- decidirAireador(oxigeno, estadoAireador)
                        estadoBomba <- decidirBomba(distAgua, humedad, faseRiego)
                        Si distAgua > 15 Entonces
                            Escribir "[SEGURIDAD] Nivel de agua bajo: bomba BLOQUEADA."
                        SiNo
                            Si humedad >= 75 Entonces
                                Escribir "[PAUSA RIEGO] Sustrato humedo (75 % o mas): bomba en espera."
                            SiNo
                                Si estadoBomba Entonces
                                    Escribir "[RIEGO ACTIVO] Bombeando agua hacia la cama de cultivo (10 seg)."
                                SiNo
                                    Escribir "[DRENAJE / REPOSO] Filtrando agua en el biofiltro (20 seg)."
                                FinSi
                                faseRiego <- NO faseRiego
                            FinSi
                        FinSi
                    FinSi
                    Si nuevoEstadoAireador Entonces
                        Escribir "Decision: aireador ENCENDIDO."
                    SiNo
                        Escribir "Decision: aireador APAGADO."
                    FinSi
                    Si nuevoEstadoAireador Y NO estadoAireador Entonces
                        contadorActivaciones <- contadorActivaciones + 1
                        Escribir "Se registra una nueva activacion."
                    FinSi
                    estadoAireador <- nuevoEstadoAireador
                    historialAireador[contadorRondas] <- estadoAireador
                    historialBomba[contadorRondas] <- estadoBomba
                FinSi
            4:
                mostrarEstadoActuadores(estadoAireador, estadoBomba, modoManual, contadorActivaciones)
            5:
                mostrarHistorial(historialMediciones, historialAireador, historialBomba, contadorRondas)
            6:
                Si contadorRondas = 0 Entonces
                    Escribir "No hay mediciones registradas para calcular estadisticas."
                SiNo
                    sumaOxigeno <- 0
                    sumaTemperatura <- 0
                    alertasCriticas <- 0
                    alertasNivel <- 0
                    minOxigeno <- historialMediciones[1,1]
                    maxOxigeno <- historialMediciones[1,1]
                    minTemperatura <- historialMediciones[1,2]
                    maxTemperatura <- historialMediciones[1,2]
                    Para i <- 1 Hasta contadorRondas Hacer
                        sumaOxigeno <- sumaOxigeno + historialMediciones[i,1]
                        sumaTemperatura <- sumaTemperatura + historialMediciones[i,2]
                        Si historialMediciones[i,1] < minOxigeno Entonces
                            minOxigeno <- historialMediciones[i,1]
                        FinSi
                        Si historialMediciones[i,1] > maxOxigeno Entonces
                            maxOxigeno <- historialMediciones[i,1]
                        FinSi
                        Si historialMediciones[i,2] < minTemperatura Entonces
                            minTemperatura <- historialMediciones[i,2]
                        FinSi
                        Si historialMediciones[i,2] > maxTemperatura Entonces
                            maxTemperatura <- historialMediciones[i,2]
                        FinSi
                        Si historialMediciones[i,1] < 3 Entonces
                            alertasCriticas <- alertasCriticas + 1
                        FinSi
                        Si historialMediciones[i,4] > 15 Entonces
                            alertasNivel <- alertasNivel + 1
                        FinSi
                    FinPara
                    promedioOxigeno <- sumaOxigeno / contadorRondas
                    promedioTemperatura <- sumaTemperatura / contadorRondas
                    Escribir "--- Estadisticas (", contadorRondas, " rondas) ---"
                    Escribir "Oxigeno promedio: ", promedioOxigeno, " mg/L"
                    Escribir "Oxigeno minimo: ", minOxigeno, " mg/L | maximo: ", maxOxigeno, " mg/L"
                    Escribir "Temperatura promedio: ", promedioTemperatura, " C"
                    Escribir "Temperatura minima: ", minTemperatura, " C | maxima: ", maxTemperatura, " C"
                    Escribir "Activaciones del aireador: ", contadorActivaciones
                    Escribir "Rondas con alerta critica (oxigeno menor a 3): ", alertasCriticas
                    Escribir "Rondas con nivel de agua bajo (distancia mayor a 15 cm): ", alertasNivel
                    Si alertasCriticas > 0 Entonces
                        Escribir "ALERTA: se detectaron niveles criticos de oxigeno."
                    FinSi
                FinSi
            7:
                Escribir "Programa finalizado."
            De Otro Modo:
                Escribir "Opcion invalida. Elija un numero del 1 al 7."
        FinSegun
    FinMientras
FinAlgoritmo
