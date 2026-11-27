/**
 *
 * @author Mathias Andres Jimenez Herrera
 */
import java.util.Locale;
import java.util.Scanner;

public class AquaTilapiaSmart {

    // ===================== FUNCIONES (devuelven un valor) =====================

    static String clasificarOxigeno(double valorOxigeno) {
        String clasificacion;
        if (valorOxigeno < 3) {
            clasificacion = "CRITICO";
        } else {
            if (valorOxigeno < 5) {
                clasificacion = "BAJO";
            } else {
                if (valorOxigeno < 6) {
                    clasificacion = "ACEPTABLE";
                } else {
                    clasificacion = "OPTIMO";
                }
            }
        }
        return clasificacion;
    }

    // true = aireador encendido. Entre 5 y 6 mg/L se mantiene el estado previo.
    static boolean decidirAireador(double valorOxigeno, boolean estadoPrevio) {
        boolean nuevoEstado;
        if (valorOxigeno < 5) {
            nuevoEstado = true;
        } else {
            if (valorOxigeno >= 6) {
                nuevoEstado = false;
            } else {
                nuevoEstado = estadoPrevio;
            }
        }
        return nuevoEstado;
    }

    // Prioridad 1: sin agua se bloquea la bomba. 2: tierra humeda, pausa. 3: temporizador.
    static boolean decidirBomba(double valorDistancia, double valorHumedad, boolean faseRiego) {
        boolean bombaEncendida;
        if (valorDistancia > 15) {
            bombaEncendida = false;
        } else {
            if (valorHumedad >= 75) {
                bombaEncendida = false;
            } else {
                bombaEncendida = faseRiego;
            }
        }
        return bombaEncendida;
    }

    // ===================== SUBPROCESOS (void) =====================

    static void mostrarMenu() {
        System.out.println("");
        System.out.println("===== CONTROL AQUATILAPIA SMART =====");
        System.out.println("1. Registrar mediciones de oxigeno, temperatura, humedad y nivel");
        System.out.println("2. Consultar el estado de la piscina");
        System.out.println("3. Evaluar y simular el control del aireador y la bomba");
        System.out.println("4. Consultar el estado del aireador y la bomba");
        System.out.println("5. Consultar el historial de mediciones y activaciones");
        System.out.println("6. Mostrar estadisticas y alertas");
        System.out.println("7. Finalizar el programa");
    }

    static void mostrarEstadoPiscina(double valorOxigeno, double valorTemperatura,
            double valorHumedad, double valorDistancia, String clasificacion) {
        System.out.println("--- Estado de la piscina ---");
        System.out.println("Oxigeno disuelto: " + valorOxigeno + " mg/L");
        System.out.println("Temperatura: " + valorTemperatura + " C");
        System.out.println("Humedad del sustrato: " + valorHumedad + " %");
        System.out.println("Distancia sensor-agua: " + valorDistancia + " cm");
        System.out.println("Clasificacion del oxigeno: " + clasificacion);
        if (valorTemperatura < 25 || valorTemperatura > 30) {
            System.out.println("[ALERTA TERMICA] Temperatura fuera del rango ideal (25 a 30 C).");
        }
        if (valorDistancia > 15) {
            System.out.println("[SEGURIDAD] Nivel de agua bajo (distancia mayor a 15 cm).");
        }
    }

    static void mostrarEstadoActuadores(boolean estadoAire, boolean estadoBomba,
            boolean modoManual, int activaciones) {
        System.out.println("--- Estado de los actuadores ---");
        if (modoManual) {
            System.out.println("Modo de operacion: MANUAL");
        } else {
            System.out.println("Modo de operacion: AUTOMATICO");
        }
        if (estadoAire) {
            System.out.println("El aireador esta ENCENDIDO.");
        } else {
            System.out.println("El aireador esta APAGADO.");
        }
        if (estadoBomba) {
            System.out.println("La bomba esta ENCENDIDA.");
        } else {
            System.out.println("La bomba esta APAGADA.");
        }
        System.out.println("Activaciones del aireador: " + activaciones);
    }

    static void mostrarHistorial(double[][] mediciones, boolean[] estadosAire,
            boolean[] estadosBomba, int cantidad) {
        String textoAire, textoBomba;
        System.out.println("--- Historial de mediciones ---");
        if (cantidad == 0) {
            System.out.println("Todavia no hay rondas registradas.");
        } else {
            for (int i = 0; i < cantidad; i++) {
                if (estadosAire[i]) {
                    textoAire = "ENCENDIDO";
                } else {
                    textoAire = "APAGADO";
                }
                if (estadosBomba[i]) {
                    textoBomba = "ENCENDIDA";
                } else {
                    textoBomba = "APAGADA";
                }
                System.out.println("Ronda " + (i + 1) + " | O2: " + mediciones[i][0]
                        + " mg/L | Temp: " + mediciones[i][1] + " C | Hum: " + mediciones[i][2]
                        + " % | Nivel: " + mediciones[i][3] + " cm | Aireador: " + textoAire
                        + " | Bomba: " + textoBomba);
            }
        }
    }

    // ===================== PROGRAMA PRINCIPAL =====================

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        sc.useLocale(Locale.US); // los decimales se escriben con punto

        int opcionMenu, modoOperacion = 0, contadorRondas, contadorActivaciones;
        int alertasCriticas, alertasNivel;
        double oxigeno, temperatura, humedad, distAgua;
        double sumaOxigeno, sumaTemperatura, promedioOxigeno, promedioTemperatura;
        double minOxigeno, maxOxigeno, minTemperatura, maxTemperatura;
        boolean estadoAireador, estadoBomba, nuevoEstadoAireador = false, modoManual, faseRiego;
        String clasificacion;

        // Matriz: fila = ronda; columnas: 0 oxigeno, 1 temperatura, 2 humedad, 3 distancia al agua
        double[][] historialMediciones = new double[20][4];
        // Arreglos: estado del aireador y de la bomba en cada ronda (true = encendido)
        boolean[] historialAireador = new boolean[20];
        boolean[] historialBomba = new boolean[20];

        opcionMenu = 0;
        contadorRondas = 0;
        contadorActivaciones = 0;
        estadoAireador = false;
        estadoBomba = false;
        modoManual = false;
        faseRiego = true;
        oxigeno = 0;
        temperatura = 0;
        humedad = 0;
        distAgua = 0;

        // Menu principal: se repite hasta elegir la opcion 7
        while (opcionMenu != 7) {
            mostrarMenu();
            System.out.println("Elija una opcion:");
            opcionMenu = sc.nextInt();

            switch (opcionMenu) {
                case 1:
                    // Registro y validacion de las lecturas
                    if (contadorRondas >= 20) {
                        System.out.println("El historial esta lleno (20 rondas). No se pueden registrar mas mediciones.");
                    } else {
                        do {
                            System.out.println("Ingrese el oxigeno disuelto (0 a 20 mg/L):");
                            oxigeno = sc.nextDouble();
                            if (oxigeno < 0 || oxigeno > 20) {
                                System.out.println("Valor invalido. Debe estar entre 0 y 20.");
                            }
                        } while (!(oxigeno >= 0 && oxigeno <= 20));
                        do {
                            System.out.println("Ingrese la temperatura (0 a 40 C):");
                            temperatura = sc.nextDouble();
                            if (temperatura < 0 || temperatura > 40) {
                                System.out.println("Valor invalido. Debe estar entre 0 y 40.");
                            }
                        } while (!(temperatura >= 0 && temperatura <= 40));
                        do {
                            System.out.println("Ingrese la humedad del sustrato (0 a 100 %):");
                            humedad = sc.nextDouble();
                            if (humedad < 0 || humedad > 100) {
                                System.out.println("Valor invalido. Debe estar entre 0 y 100.");
                            }
                        } while (!(humedad >= 0 && humedad <= 100));
                        do {
                            System.out.println("Ingrese la distancia del sensor al agua (0 a 50 cm):");
                            distAgua = sc.nextDouble();
                            if (distAgua < 0 || distAgua > 50) {
                                System.out.println("Valor invalido. Debe estar entre 0 y 50.");
                            }
                        } while (!(distAgua >= 0 && distAgua <= 50));
                        contadorRondas = contadorRondas + 1;
                        // En Java los indices inician en 0: la ronda N va en la posicion N - 1
                        historialMediciones[contadorRondas - 1][0] = oxigeno;
                        historialMediciones[contadorRondas - 1][1] = temperatura;
                        historialMediciones[contadorRondas - 1][2] = humedad;
                        historialMediciones[contadorRondas - 1][3] = distAgua;
                        historialAireador[contadorRondas - 1] = estadoAireador;
                        historialBomba[contadorRondas - 1] = estadoBomba;
                        System.out.println("Ronda " + contadorRondas + " registrada correctamente.");
                    }
                    break;
                case 2:
                    if (contadorRondas == 0) {
                        System.out.println("Primero registre una medicion (opcion 1).");
                    } else {
                        clasificacion = clasificarOxigeno(oxigeno);
                        mostrarEstadoPiscina(oxigeno, temperatura, humedad, distAgua, clasificacion);
                    }
                    break;
                case 3:
                    if (contadorRondas == 0) {
                        System.out.println("Primero registre una medicion (opcion 1).");
                    } else {
                        do {
                            System.out.println("Modo de operacion (1 = Automatico, 2 = Manual):");
                            modoOperacion = sc.nextInt();
                            if (modoOperacion != 1 && modoOperacion != 2) {
                                System.out.println("Opcion invalida. Elija 1 o 2.");
                            }
                        } while (!(modoOperacion == 1 || modoOperacion == 2));
                        modoManual = (modoOperacion == 2);
                        if (modoManual) {
                            nuevoEstadoAireador = true;
                            estadoBomba = true;
                            System.out.println("[INFO] Modo manual: aireador y bomba en flujo continuo.");
                        } else {
                            nuevoEstadoAireador = decidirAireador(oxigeno, estadoAireador);
                            estadoBomba = decidirBomba(distAgua, humedad, faseRiego);
                            if (distAgua > 15) {
                                System.out.println("[SEGURIDAD] Nivel de agua bajo: bomba BLOQUEADA.");
                            } else {
                                if (humedad >= 75) {
                                    System.out.println("[PAUSA RIEGO] Sustrato humedo (75 % o mas): bomba en espera.");
                                } else {
                                    if (estadoBomba) {
                                        System.out.println("[RIEGO ACTIVO] Bombeando agua hacia la cama de cultivo (10 seg).");
                                    } else {
                                        System.out.println("[DRENAJE / REPOSO] Filtrando agua en el biofiltro (20 seg).");
                                    }
                                    faseRiego = !faseRiego;
                                }
                            }
                        }
                        if (nuevoEstadoAireador) {
                            System.out.println("Decision: aireador ENCENDIDO.");
                        } else {
                            System.out.println("Decision: aireador APAGADO.");
                        }
                        // Se cuenta una activacion solo cuando pasa de apagado a encendido
                        if (nuevoEstadoAireador && !estadoAireador) {
                            contadorActivaciones = contadorActivaciones + 1;
                            System.out.println("Se registra una nueva activacion.");
                        }
                        estadoAireador = nuevoEstadoAireador;
                        historialAireador[contadorRondas - 1] = estadoAireador;
                        historialBomba[contadorRondas - 1] = estadoBomba;
                    }
                    break;
                case 4:
                    mostrarEstadoActuadores(estadoAireador, estadoBomba, modoManual, contadorActivaciones);
                    break;
                case 5:
                    mostrarHistorial(historialMediciones, historialAireador, historialBomba, contadorRondas);
                    break;
                case 6:
                    if (contadorRondas == 0) {
                        System.out.println("No hay mediciones registradas para calcular estadisticas.");
                    } else {
                        sumaOxigeno = 0;
                        sumaTemperatura = 0;
                        alertasCriticas = 0;
                        alertasNivel = 0;
                        minOxigeno = historialMediciones[0][0];
                        maxOxigeno = historialMediciones[0][0];
                        minTemperatura = historialMediciones[0][1];
                        maxTemperatura = historialMediciones[0][1];
                        for (int i = 0; i < contadorRondas; i++) {
                            sumaOxigeno = sumaOxigeno + historialMediciones[i][0];
                            sumaTemperatura = sumaTemperatura + historialMediciones[i][1];
                            if (historialMediciones[i][0] < minOxigeno) {
                                minOxigeno = historialMediciones[i][0];
                            }
                            if (historialMediciones[i][0] > maxOxigeno) {
                                maxOxigeno = historialMediciones[i][0];
                            }
                            if (historialMediciones[i][1] < minTemperatura) {
                                minTemperatura = historialMediciones[i][1];
                            }
                            if (historialMediciones[i][1] > maxTemperatura) {
                                maxTemperatura = historialMediciones[i][1];
                            }
                            if (historialMediciones[i][0] < 3) {
                                alertasCriticas = alertasCriticas + 1;
                            }
                            if (historialMediciones[i][3] > 15) {
                                alertasNivel = alertasNivel + 1;
                            }
                        }
                        promedioOxigeno = sumaOxigeno / contadorRondas;
                        promedioTemperatura = sumaTemperatura / contadorRondas;
                        System.out.println("--- Estadisticas (" + contadorRondas + " rondas) ---");
                        System.out.println("Oxigeno promedio: " + promedioOxigeno + " mg/L");
                        System.out.println("Oxigeno minimo: " + minOxigeno + " mg/L | maximo: " + maxOxigeno + " mg/L");
                        System.out.println("Temperatura promedio: " + promedioTemperatura + " C");
                        System.out.println("Temperatura minima: " + minTemperatura + " C | maxima: " + maxTemperatura + " C");
                        System.out.println("Activaciones del aireador: " + contadorActivaciones);
                        System.out.println("Rondas con alerta critica (oxigeno menor a 3): " + alertasCriticas);
                        System.out.println("Rondas con nivel de agua bajo (distancia mayor a 15 cm): " + alertasNivel);
                        if (alertasCriticas > 0) {
                            System.out.println("ALERTA: se detectaron niveles criticos de oxigeno.");
                        }
                    }
                    break;
                case 7:
                    System.out.println("Programa finalizado.");
                    break;
                default:
                    System.out.println("Opcion invalida. Elija un numero del 1 al 7.");
            }
        }
    }
}
