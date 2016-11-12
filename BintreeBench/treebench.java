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
	String which = args[0];
        int depth = Integer.parseInt(args[1]);
        final int numTrials = Integer.parseInt(args[2]);

	if(which.compareTo("build") == 0) {
	  long[] trials = new long[numTrials];
	  System.out.printf("Timing build tree\n");
	  System.out.printf("But first, %d warm-up iters.\n", depth, numTrials);

	  for(int i=0; i<numTrials; i++) {
            final long startTime = System.currentTimeMillis();
            Tree t2 = buildTree(depth);
            final long endTime = System.currentTimeMillis();
            if(numTrials <= 500)
                System.out.print(" " + (endTime - startTime));
          }

	  System.out.printf("\nNow for the runs we'll count:\n");
	  final long prebatch = System.currentTimeMillis();
	  for(int i=0; i<numTrials; i++) {
            final long startTime = System.currentTimeMillis();
            Tree t2 = buildTree(depth);
            final long endTime = System.currentTimeMillis();
            // Todo: use something in t2 just to be sure.
            trials[i] = (endTime - startTime);
            if(numTrials <= 500)
                System.out.print(" " + (endTime - startTime));
          }
	  final long postbatch = System.currentTimeMillis();
	  System.out.printf("\nBATCHTIME: " + ((double)(postbatch - prebatch) / 1000) + "\n");
	  Arrays.sort(trials);
	  System.out.println("\n\nMEDIANTIMED: " + Double.toString((double)trials[numTrials/2] / 1000.0));
	  double sum = 0.0;
	  for (double d : trials) sum += d;        
	  System.out.println("MEANTIMED: " + Double.toString(sum / (double)numTrials / 1000.0));

	} else {
          Tree t1 = buildTree(depth);
	  System.out.printf("Input tree built, depth %d.  Running %d iters.\n", depth, numTrials);
	  long[] trials = new long[numTrials];
	  System.out.printf("But first, %d warm-up iters.\n", depth, numTrials);

	  for(int i=0; i<numTrials; i++) {
            final long startTime = System.currentTimeMillis();
            Tree t2 = add1Tree(t1);
            final long endTime = System.currentTimeMillis();
            if(numTrials <= 500)
                System.out.print(" " + (endTime - startTime));
          }

	  System.out.printf("\nNow for the runs we'll count:\n");
	  final long prebatch = System.currentTimeMillis();
	  for(int i=0; i<numTrials; i++) {
            final long startTime = System.currentTimeMillis();
            Tree t2 = add1Tree(t1);
            final long endTime = System.currentTimeMillis();
            // Todo: use something in t2 just to be sure.
            trials[i] = (endTime - startTime);
            if(numTrials <= 500)
                System.out.print(" " + (endTime - startTime));
          }
	  final long postbatch = System.currentTimeMillis();
	  System.out.printf("\nBATCHTIME: " + ((double)(postbatch - prebatch) / 1000) + "\n");
	  Arrays.sort(trials);
	  System.out.println("\n\nMEDIANTIMED: " + Double.toString((double)trials[numTrials/2] / 1000.0));
	  double sum = 0.0;
	  for (double d : trials) sum += d;        
	  System.out.println("MEANTIMED: " + Double.toString(sum / (double)numTrials / 1000.0));
	}    
    }
}
