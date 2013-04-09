package arx.core.functions

import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: sbock
 * Date: 4/9/13
 * Time: 12:52 PM
 * To change this template use File | Settings | File Templates.
 */
object MemoizingFunction {
    class MemoizingFunction1[T,U](f: (T) => U) extends Function1[T,U] {
        val results = new mutable.HashMap[T,U]
        def apply(v1: T): U = {
            results.getOrElseUpdate(v1,f(v1))
        }
    }
    class MemoizingFunction2[S,T,U](f : (S,T) => U) extends Function2[S,T,U] {
        val results = new mutable.HashMap[(S,T),U]
        def apply(v1: S, v2: T): U = results.getOrElseUpdate((v1,v2),f(v1,v2))
    }
    class MemoizingFunction3[R,S,T,U](f : (R,S,T) => U) extends Function3[R,S,T,U] {
        val results = new mutable.HashMap[(R,S,T),U]
        def apply(v0 : R, v1: S, v2: T): U = results.getOrElseUpdate((v0,v1,v2),f(v0,v1,v2))
    }
    class MemoizingFunction4[Q,R,S,T,U](f : (Q,R,S,T) => U) extends Function4[Q,R,S,T,U] {
        val results = new mutable.HashMap[(Q,R,S,T),U]
        def apply(va : Q, v0 : R, v1: S, v2: T): U = results.getOrElseUpdate((va,v0,v1,v2),f(va,v0,v1,v2))
    }
    class MemoizingFunction5[P,Q,R,S,T,U](f : (P,Q,R,S,T) => U) extends Function5[P,Q,R,S,T,U] {
        val results = new mutable.HashMap[(P,Q,R,S,T),U]
        def apply(va : P, vb : Q, v0 : R, v1: S, v2: T): U = results.getOrElseUpdate((va,vb,v0,v1,v2),f(va,vb,v0,v1,v2))
    }
    class MemoizingFunction6[O,P,Q,R,S,T,U](f : (O,P,Q,R,S,T) => U) extends Function6[O,P,Q,R,S,T,U] {
        val results = new mutable.HashMap[(O,P,Q,R,S,T),U]
        def apply(v1: O, v2: P, v3: Q, v4: R, v5: S, v6: T): U = results.getOrElseUpdate((v1,v2,v3,v4,v5,v6),f(v1,v2,v3,v4,v5,v6))
    }
}
