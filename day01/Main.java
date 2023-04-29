import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.nio.file.*;
import java.io.IOException;
import java.lang.Math;

class FileReader {
    public String readFile(Path fp) {
        String contents = "";
        try {
            contents = new String(Files.readAllBytes(fp));
        } catch (IOException e) {
            System.out.println(e.getMessage());
            System.exit(69);
        }
        return contents;
    }
}

enum Dir {
    R, L
}

enum CompDir {
    N, E, W, S
}

class Instruction {
    Dir dir;
    int n;

    public Instruction(String rawInstr) {
        char d = rawInstr.charAt(0);
        if (d == 'R') this.dir = Dir.R;
        if (d == 'L') this.dir = Dir.L;
        this.n = Integer.parseInt(rawInstr.substring(1).trim());
    }

    public String toString() {
        return this.dir + " -> " + this.n;
    }
}

class Pos {
    int x;
    int y;

    public Pos(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public String toString() {
        return "( " + this.x + " , "  + this.y + " )";
    }

    public Pos step(int n, CompDir facing, ArrayList<Pos> trail) {
        if (facing == CompDir.N) {
            for (int i = 1; i <= n; i++) {
                trail.add(new Pos(this.x, this.y + i));
            }
            return new Pos(this.x, this.y + n);
        } else if (facing == CompDir.E) {
            for (int i = 1; i <= n; i++) {
                trail.add(new Pos(this.x + i, this.y));
            }
            return new Pos(this.x + n, this.y);
        } else if (facing == CompDir.S) {
            for (int i = 1; i <= n; i++) {
                trail.add(new Pos(this.x, this.y - i));
            }
            return new Pos(this.x, this.y - n);
        } else {
            for (int i = 1; i <= n; i++) {
                trail.add(new Pos(this.x - i, this.y));
            }
            return new Pos(this.x - n, this.y);
        }
    }

    public int manhattan(Pos other) {
        return Math.abs(this.x - other.x) + Math.abs(this.y - other.y);
    }
}

class Simulator {
    ArrayList<Instruction> instrs;
    Pos curPos;
    CompDir facing;
    static HashMap<String, CompDir> moves;
    ArrayList<Pos> vis;

    public Simulator(ArrayList<Instruction> instrs, Pos initialPos, CompDir initialFacing) {
        this.instrs = instrs;
        this.curPos = initialPos;
        this.facing = initialFacing;
        this.moves = this.getMoves();
        this.vis = new ArrayList<Pos>();
    }

    private static HashMap<String, CompDir> getMoves() {
        HashMap<String, CompDir> moves = new HashMap<String, CompDir>();
        moves.put("N , R", CompDir.E);
        moves.put("N , L", CompDir.W);
        moves.put("E , R", CompDir.S);
        moves.put("E , L", CompDir.N);
        moves.put("S , R", CompDir.W);
        moves.put("S , L", CompDir.E);
        moves.put("W , R", CompDir.N);
        moves.put("W , L", CompDir.S);
        return moves;
    }

    public void simulate() {
        for (Instruction instr : this.instrs) {
            this.step(instr);
        }
    }

    public ArrayList<Pos> firstRevisit() {
        HashSet<String> s = new HashSet<String>();
        ArrayList<Pos> revisits = new ArrayList<Pos>();
        for (Pos v : vis) {
            if (s.contains(v.toString())) revisits.add(v);
            s.add(v.toString());
        }
        return revisits;
    }

    private void step(Instruction instr) {
        this.turn(instr.dir);
        this.move(instr.n);
    }

    private void turn(Dir d) {
        this.facing = this.moves.get(this.facing + " , " + d);
    }

    private void move(int n) {
        this.curPos = this.curPos.step(n, this.facing, this.vis);
    }

    public Pos getCurPos() {
        return this.curPos;
    }
}

class FileSolver {
    Path fp;

    public FileSolver(Path fp) {
        this.fp = fp;
    }

    public void solve() {
        System.out.println("Solving file : " + fp);
        FileReader fr = new FileReader();
        String contents = fr.readFile(this.fp);
        ArrayList<Instruction> instrs = new ArrayList<Instruction>();
        for (String ins : contents.split(", ")) {
            instrs.add(new Instruction(ins));
        }
        Pos initialPos = new Pos(0, 0);
        CompDir initialFacing = CompDir.N;
        Simulator simulator = new Simulator(instrs, initialPos, initialFacing);
        simulator.simulate();
        System.out.println("part 1 : " + simulator.getCurPos().manhattan(initialPos));
        System.out.println("Part 2 : " + simulator.firstRevisit().get(0).manhattan(initialPos));
    }
}

class Main {
    public static void main(String[] args) {
        for (String fp : args) {
            FileSolver solver = new FileSolver(Paths.get(fp));
            solver.solve();
        }
    }
}
