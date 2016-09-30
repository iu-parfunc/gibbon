import java.util.Arrays;

class Tree {
}

class Node extends Tree {
    public Tree left;
    public Tree right;
    Node(Tree l, Tree r) {
        this.left = l;
        this.right = r;
    }
};

class Leaf extends Tree {
    public long elem;
    Leaf(long e) {
        this.elem = e;
    }
};

public class treebench {

    private static Tree buildTreeHelper(int n, long root) {
        if(n == 0) {
            return new Leaf(root);
        } else {
            return new Node(buildTreeHelper(n-1, root), 
                            buildTreeHelper(n-1, root + (1<<(n-1))));
        }
    }

    public static Tree buildTree(int n) {
        return buildTreeHelper(n, 1);
    }
    
    public static Tree add1Tree(Tree t) {
        if (t instanceof Leaf) {
            return new Leaf(((Leaf)t).elem + 1);
        } else {
            Node n = (Node)t;
            return new Node(add1Tree(n.left),
                            add1Tree(n.right));
        }
    }
    
    public static void main(String[] args) {        
        System.out.println("Starting.");
        int depth = 20;
        Tree t1 = buildTree(depth);
        System.out.println("First tree built.");
        final int numTrials = 33;
        long[] trials = new long[numTrials];
        for(int i=0; i<numTrials; i++) {
            final long startTime = System.currentTimeMillis();
            Tree t2 = add1Tree(t1);
            final long endTime = System.currentTimeMillis();
            trials[i] = (endTime - startTime);
            System.out.println("time(ms): " + (endTime - startTime));
        }
        Arrays.sort(trials);
        System.out.println("SELFTIMED: " + Long.toString(trials[numTrials/2]));
    }
}
