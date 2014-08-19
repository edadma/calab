package ca.hyperreal.calab


object Options extends App
{
	def apply( args: Array[String] )( options: PartialFunction[List[String], List[String]] )
	{
		def nextOption( list: List[String] ): Unit =
			if (list != Nil)
				nextOption( options(list) )

		nextOption( args.toList )
	}
	
//// test
			
	Options( args )
	{
		case "-e" :: c :: t =>
			println( "-e: " + c )
			t
		case o :: _ if o startsWith "-" => sys.error( "bad option: " + o )
		case f :: t =>
			println( "file: " + f )
			t
	}
}