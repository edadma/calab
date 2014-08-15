package ca.hyperreal.calab

import java.awt.Color
import Color._

import ca.hyperreal.color.HSL


trait CAEngineConstructor extends (String => Option[CAEngine])
{
	def string( s: String ) = s.map( _.toString.toInt ).toSet
}

trait CAEngine extends ((Int, Int, Universe) => Unit)
{
	def colors: Seq[Color]
	
	def alive: Int
}

object LifeEngine extends CAEngineConstructor
{
	val RULE1 = """B(\d*)/S(\d*)"""r
	val RULE2 = """(\d*)/(\d*)"""r
	
	def apply( rule: String ) =
	{
		if (RULE1.pattern.matcher( rule ).matches)
		{
		val RULE1(b, s) = rule
		
			Some( new LifeEngine(string(b), string(s)) )
		}
		else if (RULE2.pattern.matcher( rule ).matches)
		{
		val RULE2(s, b) = rule
		
			Some( new LifeEngine(string(b), string(s)) )
		}
		else
			None
	}
	
	override def toString = "Life"
}

class LifeEngine( birth: Set[Int], survival: Set[Int] ) extends CAEngine
{
	def apply( x: Int, y: Int, u: Universe )
	{
	var neighbours = 0
	
		for (i <- -1 to 1)
		{
			neighbours += u.read( x + i, y - 1 )
			neighbours += u.read( x + i, y + 1 )
		}
		
		neighbours += u.read( x - 1, y )
		neighbours += u.read( x + 1, y )

		u.write( x, y, if ((if (u.read( x, y ) == 0) birth else survival)( neighbours )) 1 else 0 )
	}
	
	val colors = Seq( DARK_GRAY.darker.darker, WHITE )
	
	val alive = 1
	
	override def toString = s"""Life-like [birth: {${birth.toList.sorted.mkString(",")}}, survial: {${survival.toList.sorted.mkString(",")}}]"""
}

object GenEngine extends CAEngineConstructor
{
	val RULE = """(\d*)/(\d*)/(\d*)"""r
	
	def apply( rule: String ) =
	{
		if (RULE.pattern.matcher( rule ).matches)
		{
		val RULE(s, b, c) = rule
		
			Some( new GenEngine(string(b), string(s), c.toInt) )
		}
		else
			None
	}
	
	override def toString = "Gen"
}

class GenEngine( birth: Set[Int], survival: Set[Int], count: Int ) extends CAEngine
{
	val alive = count - 1
	
	def apply( x: Int, y: Int, u: Universe )
	{
		def living( x: Int, y: Int ) = if (u.read( x, y ) == alive) 1 else 0
		
	val state = u.read( x, y )
	
		if (state > 0 && state < alive)
			u.write( x, y, state - 1 )
		else
		{
		var neighbours = 0
		
			for (i <- -1 to 1)
			{
				neighbours += living( x + i, y - 1 )
				neighbours += living( x + i, y + 1 )
			}
			
			neighbours += living( x - 1, y )
			neighbours += living( x + 1, y )
			
			if (state == 0)
				u.write( x, y, if (birth( neighbours )) alive else 0 )
			else
				u.write( x, y, if (survival( neighbours )) alive else alive - 1 )
		}
	}
	
	val colors = Seq( DARK_GRAY.darker.darker ) ++ HSL( .6, 1, 0 ).shading( count - 2, .3 ) :+ WHITE
	
	override def toString = s"""Generations [birth: {${birth.toList.sorted.mkString(",")}}, survial: {${survival.toList.sorted.mkString(",")}}, count: $count]"""
}