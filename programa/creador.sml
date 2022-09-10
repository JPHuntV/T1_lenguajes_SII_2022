(*
creador.sml
Tarea 1
lenguajes de programación
Jean Hunt
2018265223
*)  


(*getLine
E:Ninguna
S:Cadena de texto 
R:ninguna
O:Solicita al usuario una cadena de texto
*)
fun getLine () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "");


(*splitter
E:sep -> caracter en donde se hara la división, s->cadena de texto a dividir
S:Lista de cadenas de caracteres
R:sep debe ser tipo char y s tipo string
O:Recibe una cadena de texto s y la divivide en cada aparición de sep
*)
fun splitter sep s = String.tokens (fn c => c = sep) s;


(*elimNewlines
E:una cadena de caracteres  
S:cadena de caracteres si salto de linea
R:s debe ser tipo string
O:Recibe una cadena de caracteres con salto de linea y elimina este salto
*)
fun elimNewlines s = String.translate (fn #"\n" => "" | c => String.str c) s;


(*limpiarArchivo
E:ruta del archivo
S:resetea el archivo
R:ninguna
O:Solicita una archivo y limpia el indice, si el archivo no existe lo crea
*)
fun limpiarArchivo (ruta) = let
    val fd = TextIO.openOut ruta
    val _ = TextIO.output(fd,"RANKING AVISTAMIENTOS,CLASE,ORDEN,ESPECIE,ALTURA_LARGO,PESO") handle e => (TextIO.closeOut fd; raise e)
    val _ = TextIO.closeOut fd
in ()
end;


(*esInt
E:s->string
S:true si es un entero false si no 
R:s debe ser string
O:Verifica si una cadena de caracteres es un entero
*)
fun esInt s = 
    case Int.fromString(s) of
        NONE => false
        | _ => true
;


(*esReal
E:s->string
S:true si es real false si no 
R:s debe ser string
O:Verifica si una cadena de caracteres es un real
*)
fun esReal s = 
    case Real.fromString(s) of
        NONE => false
        | _ => true
;


(*solicitarValor
E:s->string, valor que está solicitando 
S:valor que ingresó el usuario 
R:el valor ingresado debe ser un string
O: Solicita un valor al usuario hasta que este ingrese una cadena de texto no numerica
*)
 fun solicitarValor (s) = let
        val _ = print ("\n"^s^": ")
        val valor = elimNewlines(getLine())          
        in 
            if (esInt(valor) orelse esReal(valor)) orelse String.size(valor)=0 then 
                let val _ = print "\nIngrese un valor no numerico!" 
                in solicitarValor(s) end
            else 
                valor
        end

 
(*solicitarNum
E:s->valor ue se está solicitando 
S:valor solicitado
R:el valor ingresado debe ser numerico
O: Solicita un valor al usuario hasta que este sea un numero 
*)
 fun solicitarNum (s) = let
        val _ = print ("\n"^s^": ")
        val num = elimNewlines(getLine())
        in 
            if esInt(num) orelse esReal(num)
                then num
            else
                let val _ = print "\nIngrese un valor valido!" 
                in solicitarNum(s) end
        end
;


(*RANKING AVISTAMIENTOS,CLASE,ORDEN,ESPECIE,ALTURA_LARGO,PESO*)
(*agregarEntrada
E:ruta del archivo donde se agregara la entrada
S:agrega una nueva linea al archivo 
R:la entrada debe cumplir con el formado
O: Solicita los valores al usuario para añadir un nuevo especimen al indice
*)
fun agregarEntrada (ruta) = let
    val fd = TextIO.openAppend ruta
   
    fun solicitarRank () = let
        val _ = print ("\nRanking: ")
        val num = elimNewlines(getLine())
        in 
            if esInt(num) 
                then Int.toString(valOf(Int.fromString(num)))
            else
                let val _ = print "\nIngrese un valor valido!" 
                in solicitarRank() end
        end
    val rank = solicitarRank();
    val clase = solicitarValor("clase")
    val orden = solicitarValor("orden")
    val especie = solicitarValor("especie")
    val altura = solicitarNum("Altura")
    val peso = solicitarNum("Peso")
    val _ = TextIO.output(fd,"\n"^rank^","^clase^","^orden^","^especie^","^altura^","^peso) handle e => (TextIO.closeOut fd; raise e)
    val _ = TextIO.closeOut fd
in ()
end;


(*imprimirMenu
E:lista de especimenes
S:llamada a la función escogida
R:Se debe elegir una opción valida
O: Despliega un menu al usuario para que este elija que operación llevar a cabo
*)
fun imprimirMenu (ruta) = let
    val menu = print "\n\n\n1.Agregar especimen\n\
                        \2.Limpiar indice\n\
                        \3.Salir\n\n\
                        \Escoja una opcion: "
    val x = elimNewlines(getLine())        
    in 
        if x = "1" then
            agregarEntrada(ruta)
        else if x = "2" then
            limpiarArchivo(ruta)
        else if x = "3" then OS.Process.exit(OS.Process.success)
        else imprimirMenu(ruta)
    end
    
;


(*main
E:ninguna
S:ejecución del programa
R:ninguna
O:Solicita la dirección del archivo e inicia el programa
*)
fun main () = 
let
    val _ = print"Ingrese la ruta del archivo: \t"
    val ruta = elimNewlines(getLine())
in 
    while true do(
        imprimirMenu(ruta)
    )
end
;