package ca.hyperreal.calab


object Options extends App
{
	type OptionFunction = PartialFunction[List[String], List[String]]

	def apply( args: Array[String] )( options: OptionFunction )
	{
		nextOption( args.toList, options )
	}
	
	private def nextOption( list: List[String], options: OptionFunction ): Unit =
		if (list != Nil)
			nextOption( options(list), options )
			
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