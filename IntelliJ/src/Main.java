import org.jpl7.Atom;
import org.jpl7.Query;
import org.jpl7.Term;
import org.jpl7.Variable;

public class Main {
    public static void main(String[] args) {
        String path = "C:\\Users\\manuc\\Dropbox\\4° Año, Segundo Cuatrimestre 2018\\Inteligencia Artificial\\Trabajo Final\\Binairo\\binairo.pl";
        Query q1 = new Query("consult", new Term[]{new Atom(path)});
        if (q1.hasSolution()) {
            System.out.println("Programa listo para consultar...");
        } else {
            System.out.println("Archivo no encontrado.");
        }

        Variable X = new Variable();
        // Query q2 = new Query("siguiente", new Term[] { new Atom("3"), X });
        Query q2 = new Query("siguiente(3, X)");
        // Map<String, Term> solution = q2.oneSolution();
        System.out.println("Solutions of siguiente(3, X)");
        while (q2.hasMoreSolutions()) {
            String solucion = q2.nextSolution().toString();
            solucion = solucion.substring(1, solucion.length() - 1);
            System.out.println(solucion);
        }
    }
}
