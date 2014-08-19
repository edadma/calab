package ca.hyperreal.calab

import java.awt.Color
import Color._

import ca.hyperreal.color.HSL


trait CAEngineHelper
{
	def string( s: String ) = s.map( _.toString.toInt ).toSet
}

trait CAEngineConstructor
{
	def instance( rule: String ): Option[CAEngine]
}

trait CAEngine
{
	def update( x: Int, y: Int, u: Universe ): Unit
	
	def colors: Array[Color]
	
	def maxValue: Int
}

object LifeEngine extends CAEngineConstructor with CAEngineHelper
{
	val RULE1 = """B(\d*)/S(\d*)"""r
	val RULE2 = """(\d*)/(\d*)"""r
	
	def instance( rule: String ) =
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
	def update( x: Int, y: Int, u: Universe )
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
	
	val colors = Array( DARK_GRAY.darker.darker, WHITE )
	
	val maxValue = 1
	
	override def toString = s"""Life-like [birth: {${birth.toList.sorted.mkString(",")}}, survial: {${survival.toList.sorted.mkString(",")}}]"""
}

object GenEngine extends CAEngineConstructor with CAEngineHelper
{
	val RULE = """(\d*)/(\d*)/(\d*)"""r
	
	def instance( rule: String ) =
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
	val maxValue = count - 1
	
	def update( x: Int, y: Int, u: Universe )
	{
		def living( x: Int, y: Int ) = if (u.read( x, y ) == maxValue) 1 else 0
		
	val state = u.read( x, y )
	
		if (state > 0 && state < maxValue)
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
				u.write( x, y, if (birth( neighbours )) maxValue else 0 )
			else
				u.write( x, y, if (survival( neighbours )) maxValue else maxValue - 1 )
		}
	}
	
	val colors = Array( DARK_GRAY.darker.darker ) ++ HSL.shading( .6, 1, count - 2, .3 ) :+ WHITE
	
	override def toString = s"""Generations [birth: {${birth.toList.sorted.mkString(",")}}, survial: {${survival.toList.sorted.mkString(",")}}, count: $count]"""
}
