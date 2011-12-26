BEGIN { FS = " "; str = ""; }
{
  for (i=1; i<=NF; i++)
    {
      if (substr($i, 2, 1)=="L") 
	str = sprintf("%s %s", str, $i)
	  }
  for (i=1; i<=NF; i++)
    {
      if (substr($i, 2, 1)!="L") 
	str = sprintf("%s %s", str, $i)
	  }
} END { print str }
