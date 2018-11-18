package binairo;

public class Celda {

    private final int i;
    private final int j;
    private final String c;

    public Celda(int i, int j, String c) {
        this.i = i;
        this.j = j;
        this.c = c;
    }

    public int getI() {
        return i;
    }

    public int getJ() {
        return j;
    }

    public String getC() {
        return c;
    }

    @Override
    public String toString() {
        return "Celda{" + "i=" + i + ", j=" + j + ", c=" + c + '}';
    }
}
