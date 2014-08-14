package ca.hyperreal.calab

import java.awt.Color
import Color._
import ca.hyperreal.color.HSL


class VoteForLife extends CAEngineConstructor
{
	val RULE = """(\d+)"""r
	
	def apply( rule: String ) =
	{
		if (RULE.pattern.matcher( rule ).matches)
		{
		val RULE(l) = rule
		
			Some( new VoteForLifeEngine(string(l)) )
		}
		else
			None
	}
	
	override def toString = "Vote"
}

class VoteForLifeEngine( lives: Set[Int] ) extends CAEngine
{
	def apply( x: Int, y: Int, u: Universe )
	{
	var neighbours = 0
	
		for (i <- -1 to 1; j <- -1 to 1)
			neighbours += u.read( x + i, y + j )

		u.write( x, y, if (lives( neighbours )) 1 else 0 )
	}
	
	val colors = Seq( DARK_GRAY.darker.darker, WHITE )
	
	val alive = 1
	
	override def toString = s"""Vote for Life [lives: {${lives.toList.sorted.mkString(",")}}]"""
}