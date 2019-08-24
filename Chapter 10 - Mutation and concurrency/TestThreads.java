import java.util.concurrent.*;

/**
 * Java code to try submitting threads with closure-like code to ExecutorService
 */
 
interface MyRunnable {

   void run(int x, int y);

}

class TestThreads {

   static int getAvailProcs() {
      return Runtime.getRuntime().availableProcessors();
   }

   static ExecutorService createThreadPool() {
      return Executors.newFixedThreadPool(getAvailProcs() + 2);
   } 

   static void doThreads(ExecutorService threadPool,MyRunnable foo,final int threadCount,final int timesCount) {
      for (int i=0;i<threadCount;i++) {
         // we need to create a context in which "i" is "effectively final" in order to use "i" in anonymous inner class
         doThreadsInner(threadPool,foo,i,timesCount);
     }
   }

   static void doThreadsInner(ExecutorService threadPool,MyRunnable foo,final int i,final int timesCount) {
      // Signature of submit(): "<T> Future<T> submit(Callable<T> task)"
      // ...and Callable<V> is an "functional interface" with a single function: "V call()"
      // We are not interested in returning anything, so we parametrize with Void and return null.
      threadPool.submit(new Callable<Void>() {
         public Void call() {
            for (int j=0;j<timesCount;j++) {
               foo.run(i,j); 
            }
            return null;
         }
      });
   }

   public static void main(String[] argv) {
      ExecutorService xs = createThreadPool();

      MyRunnable mr = new MyRunnable() {
         public void run(int x,int y) {
            System.out.println("thread: " + x + " count: " + y);
            try {
               Thread.sleep(1000);
            } 
            catch (Exception exe) {
               // suppress
            }
            if (Math.random() > 0.5) {
               System.out.println("thread: " + x + " count: " + y + " ADIOS!");
               throw new IllegalStateException("No throwing in thread " + x + " count " + y);
            }
            else {
               System.out.println("thread: " + x + " count: " + y + " ends normally");
            } 
         }
      };

      doThreads(xs,mr,100,100);
      xs.shutdown();
      try {
         xs.awaitTermination(10,TimeUnit.SECONDS);
      }
      catch (InterruptedException exe) {
         System.err.println(exe);
      }
   }

}

