// $Id: Sinc.java,v 1.1 2004/10/26 19:20:23 greg Exp $

public class Sinc
{
  static public double sinc( double t ) {
    if (t==0.0)
      return 1;
    t *= Math.PI;
    return Math.sin( t ) / t;
  }

  static public void main( String args[] ) throws Exception {
    int len = Integer.parseInt( args[0] );
    int ncyc = Integer.parseInt( args[1] );
    String outfile = args[2];

    Signal signal = new Signal( len );
    double raw[][] = signal.raw();
    for (int i=0; i<len; ++i) {
      double a =
        (((double)i-(((double)len-1)/2))*(double)ncyc)/(((double)len-1)/2);
      double s = sinc( a );
//System.out.println( "a "+a+" s " +s );
      raw[0][i] = s;
    }

    SoundSignal.convert( signal ).saveTo( outfile );
  }
}
