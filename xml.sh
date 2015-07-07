for i in /media/backup/data/treebank/parsed/*
  do
  { 
      j=`basename $i.xml`;
      echo $j;
      cat $i/*.mrg > data/$j; 
  }
done
