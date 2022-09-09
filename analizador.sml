

fun getLine () = Option.getOpt (TextIO.inputLine TextIO.stdIn, "");

fun splitter sep s = String.tokens (fn c => c = sep) s;

fun elimNewlines s = String.translate (fn #"\n" => "" | c => String.str c) s;

fun printRes(lista) = let
    val _ = print "\nRANKING AVISTAMIENTOS |\t\tCLASE |\t\t\tORDEN |\t\t\t  ESPECIE |\t    ALTURA_LARGO |\t\t    PESO |\n\
                    \___________________________________________________________________________________________________________________________________________"
    val _ = List.map(fn x =>
                print ("\n" ^Format.format "%21s" [Format.STR (List.nth(x,0))]  ^
                      " |\t"^Format.format "%13s" [Format.STR (List.nth(x,1))]  ^
                      " |\t"^Format.format "%21s" [Format.STR (List.nth(x,2))]  ^
                      " |\t"^Format.format "%25s" [Format.STR (List.nth(x,3))]  ^
                      " |\t"^Format.format "%16s" [Format.STR (List.nth(x,4))]  ^
                      " |\t"^Format.format "%16s" [Format.STR (List.nth(x,5))]  ^ " |"
                )
            ) lista
    in 
        [[""]]
    end
;




fun listOf(nil) = nil
|   listOf(x::xs) =  String.tokens (fn c => c = #",") x::listOf(xs)
;


fun abrirArchivo () = let 
    val infile =  elimNewlines(getLine())
    val ins = TextIO.openIn infile 
    fun loop ins = 
        case TextIO.inputLine ins of 
            SOME line => elimNewlines(line) :: loop ins 
            | NONE      => [] 
    in 
        loop ins before TextIO.closeIn ins 
    end
;


(*(**)Elimina el elemento k de una lista*)
fun delete_ith ([], k) = []
  | delete_ith (x::xs, 1) = xs
  | delete_ith (x::xs, k) = x :: delete_ith (xs, k - 1)
;






fun topAscendenteRango(lista) = let
    fun solicitarInicio () = let
        val _ = print "\nInicio: "
        val inicio = valOf(Int.fromString(elimNewlines(getLine())))
        in
            if inicio <0 then 0
            else inicio
        end

    val inicio = solicitarInicio()

    fun solicitarFinal ()= let
        val _ = print "\nFinal: "
        val final = valOf(Int.fromString(elimNewlines(getLine())))
        in
            if final > List.length(lista) then List.length(lista)
            else final
        end
    val final = solicitarFinal()
    val res = List.drop(List.take(lista,final),inicio)
    val _ = printRes(res)
    in
        res
    end
;


fun detallesAltura(lista) = let
    val _ = print "\nAltura a filtrar: "
    val altura = valOf(Real.fromString(elimNewlines(getLine())))
    val filtrados = List.filter(fn x => valOf(Real.fromString(List.nth(x,4))) > altura) lista
    val res = ListMergeSort.sort (fn(x,y)=> valOf(Real.fromString(List.nth(x,4))) <
                                    valOf(Real.fromString(List.nth(y,4)))) filtrados
    val _ = printRes(res)
    in
        res
    end
;

fun especiesRanking(lista) = let
    val _ = print "\nPosicion del ranking: "
    val posicion = valOf(Int.fromString(elimNewlines(getLine())))
    val filtrados = List.filter(fn x => valOf(Int.fromString(hd x)) = posicion) lista
    val _ = printRes(filtrados)
    in
        filtrados
    end
;


fun especiesClase(lista) = let
    val _ = print "\nClase a filtrar: "
    val clase = elimNewlines(getLine())
    val filtrados = List.filter(fn x => 
                                String.map(fn y => Char.toUpper(y)) (List.nth(x,1)) = 
                                String.map(fn z => Char.toUpper(z)) clase
                            ) lista
    val _ = printRes(filtrados)
    in
        filtrados
    end
;

fun especiesOrden(lista) = let
    val _ = print "\nOrden a filtrar: "
    val orden = elimNewlines(getLine())
    val filtrados = List.filter(fn x => 
                                String.map(fn y => Char.toUpper(y)) (List.nth(x,2)) = 
                                String.map(fn z => Char.toUpper(z)) orden
                            ) lista
    val _ = printRes(filtrados)                        
    in
        [[Int.toString(List.length(filtrados))]]
    end
;

fun eliminarEspeciesRep [] = []
  | eliminarEspeciesRep (x::xs) = x::eliminarEspeciesRep(List.filter (fn y => 
                                                                        String.map(fn y => Char.toUpper(y)) (List.nth(y,3)) <> 
                                                                        String.map(fn z => Char.toUpper(z)) (List.nth(x,3))
                                                                    ) xs)
  ;

fun eliminarRep [] = []
  | eliminarRep (x::xs) = x::eliminarRep(List.filter (fn y => y <> x) xs)
;

fun resumen (lista)= let
    val cantEspecies = eliminarEspeciesRep(lista)
    val especieTamanioNombre = ListMergeSort.sort (fn(x,y)=> String.size(List.nth(x,3)) <
                                    String.size(List.nth(y,3))) lista
    val especiesPeq = List.filter(fn x => 0.0 <= valOf(Real.fromString(List.nth(x,4)))
                                        andalso valOf(Real.fromString(List.nth(x,4))) <=2.5) lista
    
    val especiesMed = List.filter(fn x => 2.6 <= valOf(Real.fromString(List.nth(x,4)))
                                        andalso valOf(Real.fromString(List.nth(x,4))) <=5.0) lista

    val especiesGrandes = List.filter(fn x => valOf(Real.fromString(List.nth(x,4))) > 5.0) lista

    val espXClase = eliminarRep( List.map (
        fn x => [List.nth(x,1)] @ 
                [Int.toString(List.length(
                    List.filter(fn y => List.nth(y,1) = List.nth(x,1)) lista
                ))]
        )lista)

    val _ = print(
        "\nCantidad de especies: " ^ Int.toString(List.length(cantEspecies)) ^
        "\nEspecie con el nombre mas largo: " ^ List.nth(hd especieTamanioNombre,3) ^
        "\nCantidad de especies pequeñas: " ^ Int.toString (List.length(especiesPeq)) ^
        "\nCantidad de especies medianas: " ^ Int.toString (List.length(especiesMed)) ^
        "\nCantidad de especies grandes: " ^ Int.toString (List.length(especiesGrandes)) ^
        "\n\nCantidad de especies por clase\n"
    )

    val _ = print "\n\tClase |\t\tCantidad |\n_____________________________________________________"
    val _ = List.map(fn x =>
                print ("\n" ^ Format.format "%13s" [Format.STR (List.nth(x,0))] ^
                        " |\t" ^ Format.format "%16s" [Format.STR (List.nth(x,1))] ^ " |")
            )espXClase

    val _ = print"\n"
    in
        [[""]]
    end
;

fun imprimirMenu (lista) = let
    val menu = print "\n\n\n1.Mostrar top ascendente por rango\n\
                        \2.Mostrar detalles animales por altura\n\
                        \3.Mostrar detalles especies\n\
                        \4.Mostrar especies por clase\n\
                        \5.Cantidad de especies por orden\n\
                        \6.Resumen\n\
                        \7.Salir\n\n\
                        \Escoja una opcion: "
    val x = elimNewlines(getLine())        
    in 
        if x = "1" then
            topAscendenteRango(lista)
        else if x = "2" then
            detallesAltura(lista)
        else if x = "3" then
            especiesRanking(lista)
        else if x = "4" then
            especiesClase(lista)
        else if x = "5" then
            especiesOrden(lista)
        else if x = "6" then
            resumen(lista)
        else if x = "7" then OS.Process.exit(OS.Process.success)
        else imprimirMenu(lista)
    end
    
;


fun main () = 
let
    val _ = print"Ingrese la ruta del archivo: \t"
    val lista = delete_ith (abrirArchivo(), 1)
    val lista2= listOf(lista)
    val order = ListMergeSort.sort (fn(x,y)=> valOf(Int.fromString (hd x)) >
                                    valOf(Int.fromString(hd y))) lista2
in 
    while true do(
        imprimirMenu(order)
    )
end
;
