fun getLine () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "");

fun splitter sep s = String.tokens (fn c => c = sep) s;

fun elimNewlines s = String.translate (fn #"\n" => "" | c => String.str c) s;

fun limpiarArchivo (ruta) = let
    val fd = TextIO.openOut ruta
    val _ = TextIO.output(fd,"RANKING AVISTAMIENTOS,CLASE,ORDEN,ESPECIE,ALTURA_LARGO,PESO") handle e => (TextIO.closeOut fd; raise e)
    val _ = TextIO.closeOut fd
in ()
end;


fun esInt s = 
    case Int.fromString(s) of
        NONE => false
        | _ => true
;

fun esReal s = 
    case Real.fromString(s) of
        NONE => false
        | _ => true
;


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
(*RANKING AVISTAMIENTOS,CLASE,ORDEN,ESPECIE,ALTURA_LARGO,PESO*)
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