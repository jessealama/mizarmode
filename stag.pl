#!/usr/bin/perl 

# stag.pl ... create symbol tags for mizar abstracts
# run it in directory $MIZFILES/abstr on the abstracts you want, usually just
# "./stag.pl *.abs"
# relies on .dno files in $MIZFILES/prel directory and the file $MIZFILES/mml.vct
 


$prel=$ENV{MIZFILES}."/prel/";
$all="KORVMGUL";                        # this is used for indexing
# this maps symbols from $all to syntax symbols; used for defh in defshash() 
@symb=  ("func", "func", "pred", "attr", "mode", "struct", "sel", "struct");

# create the old miztags, modify into REFTAGS and delete
print "Creating reference tags\n";  
system "etags   --language=none  --regex='/ *scheme[ \n]*\\([^ {]*\\)[ \n]*{/\\1/'        --regex='/.*:: \\([^ \n:]+\\):\\([0-9]+\\)/\\1:\\2/'   --regex='/.*:: \\([^ \n:]+\\):def *\\([0-9]+\\)/\\1:def \\2/' *.abs";
open(IN, "TAGS");
open(OUT,'>REFTAGS'); 
while (<IN>) { s/.*(.*)/$1;$1/; print OUT $_;};
close(IN); 
close(OUT);
system "rm TAGS";
print "REFTAGS done\n";

# read in vocabularies into %voch
print "Reading in vocabulary information\n";
getnames();
print "Vocabulary information read\n";

# the main loop
print "Creating symbol tags\n";
sleep 1;
open(OUT,'>SYMBTAGS');
while ($file = shift) {
    ($fnoext) = split(/\./,$file);
    print "$fnoext\n";
    $nfile= $prel.substr($file,0,1)."/".$fnoext.".dno";
    open(IN, $nfile) or next;
    # this creates %defh...for each "func "pred" etc ... list of symbols from .dno
    defshash();                                  
    foreach $key (keys %defh) {$bound{$key} = $#{$defh{$key}}};  # %bound holds the counts
    close(IN);        
    open(IN, $file);                                             # opening .abs 
    $bytes=0;                                                    # counting bytes for tags
    print OUT "\n$file,\n";                                        # tag file header
    while (<IN>) 
    { $l=length($_);                                             # bytes counting
      chop($_);
      s/::.*//;                                                  # strip comments
# beware, more defs can be on one line! ... ok
	  while(m/\b(func|pred|attr|struct|mode|synonym|antonym)\b/g) {
	      $found=$1;
              # guess synonyms and antonyms            
	      if (($found eq "synonym")||($found eq "antonym")) { 
		  if (($bef eq "pred")||($bef eq "attr")) { $j = guesswhat();}
		  else {$j = $bef}}
	      else {$bef = $found; $j = $found};
	      shiftprint();                                            # shift and print tag
              # for structs, print defined sels if any
	      if (($found eq "struct") && (defined($selh{$sname})))  {
		  $str = $sname;
		  $j="sel";
		  foreach $i (0 .. $#{$selh{$str}} ) { shiftprint();} 
	      }  
	  }
      $bytes+= $l;                                                     # increase byte count
  }
    close(IN);                                                         
}

print "SYMBTAGS tags done\n";
# end of main loop

# this reads the vocs form mml into the global hash %voch
sub getnames {
    open(VOC, $ENV{MIZFILES}."/mml.vct");
    undef %voch;
    $_=<VOC>; 
    m/\#([A-Z0-9_-]+).*$/;
    $aname=$1;
    <VOC>;
    $newaname=$aname;
    while ($newaname) {                                 # while onevoc() finds new voc name 
	undef $newaname;                                # call it to read its symbols 
	$voch{$aname}= onevoc();
	$aname=$newaname;
    }
    correcthidden();                                    # adds builtins to hidden
    close(VOC);
}


# this reads one voc from mml into 2-dimensional $arr and returns it
sub onevoc {
    my $arr; my $p;
    $arr=[[],[],[],[],[],[],[],[]];
    while (<VOC>) {
	if (/^\#([A-Z0-9_-]+).*$/) {$newaname=$1; $_=<VOC>; return $arr;}
	m/^([KORVMGUL])([^ \n\r]+) *.*$/;
	$p=index($all,$1);
        $arr->[$p][1+$#{$arr->[$p]}]=$2;
    }
    return $arr
}

# add builtin symbols for the HIDDEN vocabulary
sub correcthidden {
    @Ks=("[", "{"); @Rs=("="); @Ms=("set");
    unshift  @{$voch{"HIDDEN"}->[0]}, @Ks;
    unshift  @{$voch{"HIDDEN"}->[2]}, @Rs;
    unshift  @{$voch{"HIDDEN"}->[4]}, @Ms;
}

# %defh...for each symbol "func "pred" "mode" ...contains list of symbols
# create it from .dno file, special care for structs... we need constr too to
# recognize its selectors
sub defshash {
    %defh = ( "func" => [], "pred" => [], "mode" => [], 
	      "attr" => [], "sel" => [], "struct" => []);
    do { $_=<IN>;} until /\#/;
    $hidden=0;                                       # if using HIDDEN, ban V1 (="strict") 
    undef(%selh);                                    # for each struct constr array of its sels  
    undef(%struct);                                  # constr structs names
    maketransl();                                    # table for relative names in this .dno
    while (<IN>) {
	if (/^([�����]).*$/) {
	    if ($1 eq "�") { @Gs = /(G[0-9]+)/g; $g = $Gs[$#Gs]; }  # if "U" def, get its  struct 
	    else { undef $g };
	    $_=<IN>;
	    m/^([KORVMGU])([0-9]+).*$/ or print "st not found in $nfile: $_";
	    if (($1 eq "V")&&($2 == 1)&&($hidden==1)) { next}; # strict not visible
	    $r = $2; $s = $1;
	    if (defined($g)) { pushsel()};		   # push "U" into its struct array in %selh
	    $p = index($all,$1);
	    $f = $defh{$symb[$p]};	                   # array of the syntax symbol
	    $f->[1+$#{$f}] = $trans->[$p][$r-1];           # put the string there
	    if ($s eq "G") {                               # need name of struct constr
		<IN>; $_=<IN>;                             
		m/^(G[0-9]+)/ or print "constr not found in $nfile: $_";
		$struct{$1} = $f->[$#{$f}];                # keeps names of struc constrs
	    }
		
	}}
    return %defh;
}

# $trans...tells the voc name and the number in that voc of a given ralative symbol
# this creates the trans table for one .dno
sub maketransl {
    $trans=[[],[],[],[],[],[],[],[]];
    while (<IN>) { 
	if (/^\#/) {return $trans;};                    # ending hash found
	m/^v([A-Z0-9_-]+).*$/ or print "Error1";
	if ($1 eq "HIDDEN") { $hidden=1};
	foreach $i (0 .. 7) {
	    foreach $j (0 .. $#{$voch{$1}->[$i]}) {
		$trans->[$i][1+$#{$trans->[$i]}] = $voch{$1}->[$i][$j];
	    }}
	$_=<IN>;
    }
}

# pushes sel $r into its struct constr array
sub pushsel {
    undef $str;
    $str=$struct{$g};                                  # this yields the name of the struct constr
    if (!defined($selh{$str})) { $selh{$str} = [] }; 
    $selh{$str}->[1+$#{$selh{$str}}] = $r;
}

# guesses synonyms amd antonyms
# if \bis\b inside, then attribute, otherwise pred
sub guesswhat {
   $g=substr($_,pos);
   ($g1)=m/([^;]*)/;
   if ($g1=~m/\bis[ \n\r]/) { return "attr" }
   else {return "pred"}
}

# shifts the array in $defh{$j} and does the tag printing
sub shiftprint {
    $sname = shift @{$defh{$j}};
    $ord = $bound{$j}-$#{$defh{$j}};
    print OUT $sname.";".uc($fnoext).":$j:$ord$sname$.,$bytes\n";
}
