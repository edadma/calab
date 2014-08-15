package ca.hyperreal.calab


class Pattern( val width: Int, val height: Int )
{
	private val array = Array.fill[Int]( width, height )( 0 )
	
	def get( x: Int, y: Int ) = array(x)(y)
	
	def put( x: Int, y: Int, v: Int ) = array(x)(y) = v
}