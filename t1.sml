

fun getLine () =
  Option.getOpt (TextIO.inputLine TextIO.stdIn, "");

fun splitter sep s = String.tokens (fn c => c = sep) s;


fun elimNewlines s =
       String.translate (fn #"\n" => "" | c => String.str c) s;
    


fun printLista(lista) = 
    if lista = [] then print("\n") else
    let 
        val _ = print((hd lista)^"\n")
    in
        printLista(tl lista)
    end;


fun printLista2(lista) = 
    if lista = [] then print("\n") else
    let 
        val _ = printLista((hd lista))
    in
        printLista2(tl lista)
    end;


fun listOf(nil) = nil
|   listOf(x::xs) =  String.tokens (fn c => c = #",") x::listOf(xs);


fun abrirArchivo () = let 
    val infile =  elimNewlines(getLine())
    val ins = TextIO.openIn infile 
    fun loop ins = 
    case TextIO.inputLine ins of 
        SOME line => elimNewlines(line) :: loop ins 
        | NONE      => [] 
    in 
    loop ins before TextIO.closeIn ins 
    end ;


(*(**)Elimina el elemento k de una lista*)
fun delete_ith ([], k) = []
  | delete_ith (x::xs, 1) = xs
  | delete_ith (x::xs, k) = x :: delete_ith (xs, k - 1);






fun topAscendenteRango(lista) =let
    
    fun solicitarInicio ()= let
        val _ = print "\nInicio: "
        val inicio = valOf(Int.fromString(elimNewlines(getLine())))
        in (
            if inicio <0 then 0
            else inicio
        ) end
    val inicio = solicitarInicio()

    fun solicitarFinal ()= let
        val _ = print "\nFinal: "
        val final = valOf(Int.fromString(elimNewlines(getLine())))
        in (
            if final > List.length(lista) then List.length(lista)
            else final
        ) end
    val final = solicitarFinal()
    in
        List.drop(List.take(lista,final),inicio)
    end
;


fun detallesAltura(lista) = let
    val _ = print "\nAltura a filtrar: "
    val altura = valOf(Real.fromString(elimNewlines(getLine())))
    val filtrados = List.filter(fn x => valOf(Real.fromString(List.nth(x,4))) > altura) lista
    

    in
        ListMergeSort.sort (fn(x,y)=> valOf(Real.fromString(List.nth(x,4))) <
                                    valOf(Real.fromString(List.nth(y,4)))) filtrados
    end
;

fun especiesRanking(lista) = let
    val _ = print "\nPosición del ranking: "
    val posicion = valOf(Int.fromString(elimNewlines(getLine())))
    val filtrados = List.filter(fn x => valOf(Int.fromString(hd x)) = posicion) lista
    in
        filtrados
    end
;


fun especiesClase(lista) = let
    val _ = print "\nClase a filtrar: "
    val clase = elimNewlines(getLine())
    val filtrados = List.filter(fn x => List.nth(x,1) = clase) lista
    in
        filtrados
    end
;

fun imprimirMenu (lista) = let
    val menu = print "\n1.**Mostrar top ascendente por rango\n\
                        \2.Mostrar detalles animales por altura\n\
                        \3.Mostrar detalles especies\n\
                        \4.Mostrar especies por clase\n\
                        \5.Cantidad de especies por orden\n\
                        \6.Resumen\n\
                        \7.Salir\n\n\
                        \Escoja una opción: "
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
    else if x = "7" then OS.Process.exit(OS.Process.success)
    else imprimirMenu(lista)
     end
    
;






fun tarea1 () = 
let
    val _ = print"Ingrese la ruta del archivo: \t"
    val lista = delete_ith (abrirArchivo(), 1)
    val lista2= listOf(lista)
    val order = ListMergeSort.sort (fn(x,y)=> valOf(Int.fromString (hd x)) >
                                    valOf(Int.fromString(hd y))) lista2
in( 
    printLista(lista),
    printLista2( lista2),
    imprimirMenu(order)
    
)
end
;