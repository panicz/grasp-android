import java.io.*;


// teraz: jak sie w ogole zabrac za kwestie konwersji
// SExp do Bit?
// - SList konwertujemy do Box
// - jezeli SList jest niepusty, tworzymy jedna instancje
//   Line
// - jezeli preceding_whitespace kolejnego obiektu
//   zawiera nowe linie, to tworzymy nowa linie,
//   dodajac do Boxa odpowiednia wysokosc interlinii
// 


class SExpParserExample {
    public static void main(String [] arguments) {
	try {
	    Reader input = new InputStreamReader(System.in);
	    
	    SExpReader s = new
		SExpReader(new PeekingReader(input, 4));
			   
	    SExp x = s.read_expression();
	    System.out.println(x);
	}
	catch(IOException e) {
	    System.out.println(e);
	}
    }
}
