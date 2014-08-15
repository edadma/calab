package ca.hyperreal

import java.awt.{Dimension}


package object calab
{
	implicit def pair2dimension( pair: (Int, Int) ) = new Dimension( pair._1, pair._2 )
}