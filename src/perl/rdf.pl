#!/usr/bin/perl
use strict;

#print $_ . "\n" foreach @INC;

#use RDF::Query::Client;
use XML::Simple;
use RDF::Trine::Parser;
use Data::Dumper;
use TempFileNames;
use URI;
use LWP::Simple;
use Getopt::Long;
use Pod::Usage;
use Switch;

my $man = 0;
my $help = 0;

my $options = {
	'endpoint' => 'http://bind.bio2rdf.org/sparql',
	#'templ-file' => 'templ.txt',
	'outfile' => 'outfile.txt',
	};
 
sub kegg
{
        my $list=shift;

	my $query="construct {?gene1 <http://bio2rdf.org/geneid_resource:src> ?kegg .} where {";
	my $block =" { ?gene1 a <http://bio2rdf.org/geneid_resource:Gene> .
		?gene1 <http://bio2rdf.org/geneid_resource:src> ?kegg.
		filter regex(?kegg, 'pathway:";
	foreach (@$list)
	{
		$query.=$block . $_ ."') .} union";
	}
	substr($query,length($query)-6,6,"");
	$query.="}";
	$query =~ s/\n/ /g;
	$query =~ s/\s+/ /g;
	return $query;
}


sub interaction
{       
	my $list=shift;
	my $query="construct {?gene1 <http://bio2rdf.org/geneid_resource:interactWith> ?gene2 .} where {";
	my $block=" { ?gene1 <http://bio2rdf.org/geneid_resource:src> ?gene2 . 
	?gene1 a <http://bio2rdf.org/geneid_resource:Gene> . 
	?gene2 a <http://bio2rdf.org/geneid_resource:Gene> . 
	filter (?gene1=<http://bio2rdf.org/geneid:";
	foreach (@$list)
	{
		$query.=$block . $_ .">) .} union";
	}
	substr($query,length($query)-6,6,"");
	foreach (@$list) {
			$query .= " filter (?gene2 != <http://bio2rdf.org/geneid:" . $_ . ">) .";
	}
	$query.="}";
	$query =~ s/\n/ /g;
	$query =~ s/\s+/ /g;
	return $query;	
}

sub omim
{
	my $list=shift;
	my $par=shift; #1 - word, 0 - id;
	my $query="construct {?omimID <http://bio2rdf.org/hgnc_resource:entrezGeneID> ?geneID}
		where {";
	my $block =" { ?hgncID <http://bio2rdf.org/hgnc_resource:OMIMID_mappeddatasuppliedbyNCBI> ?omimLink .
		?omimLink <http://bio2rdf.org/hgnc_resource:xOMIM> ?omimID .
		?hgncID <http://bio2rdf.org/hgnc_resource:entrezGeneID> ?geneID .
		filter (?omimID = <http://bio2rdf.org/omim:";
	if ($par==0)
	{
		foreach (@$list)
		{
			$query.=$block . $_ .">) .} union";
		}
	substr($query,length($query)-6,6,"");
	$query.="}";
	}
	$query =~ s/\n/ /g;
	$query =~ s/\s+/ /g;
	return $query;
}


sub goWord
{
	my $list=shift;
	my $query="construct {?go a <http://bio2rdf.org/ns/go#Term>}
		where {";
	my $block1 =" {
		?go a <http://bio2rdf.org/ns/go#Term> .
		?go rdfs:label ?golabel .
		filter regex(?golabel,'";
	my $block2="').} union {
		?go a <http://bio2rdf.org/ns/go#Term> .
		?go <http://bio2rdf.org/ns/bio2rdf#synonym> ?gosyn .
		filter regex(?gosyn, '";
	foreach (@$list)
	{
		$query.=$block1 . $_  . $block2 . $_ . "' ).} union";
	}
	substr($query,length($query)-6,6,"");
	$query.="}";
	$query =~ s/\n/ /g;
	$query =~ s/\s+/ /g;
	my $triples=&sparqlQuery("http://go.bio2rdf.org/sparql", $query);
	
	my $iterator = $triples->as_stream;

	my @goTerm;

	while (my $row = $iterator->next) {
		$row->subject =~ m#go\:(\d*)\>#;
		push(@goTerm, $1); 
	}
	die "no GO terms were found \n" if (@goTerm==0);

	#print @goTerm . "\n";
	return \@goTerm;

}


sub go
{
	my $list=shift;
	my $par=shift; #1 - word, 0 - id;

   	$list=goWord($list) if ($par);

	my $query="construct {?gene1 <http://bio2rdf.org/geneid_resource:src> ?go .} where {";
	my $block =" { ?gene1 a <http://bio2rdf.org/geneid_resource:Gene> .
		?gene1 <http://bio2rdf.org/geneid_resource:src> ?go.
		filter (?go=<http://bio2rdf.org/go:";
	foreach (@$list)
	{
		$query.=$block . $_ .">) .} union";
	}
	substr($query,length($query)-6,6,"");
	$query.="}";
	$query =~ s/\n/ /g;
	$query =~ s/\s+/ /g;
	return $query;
}
       

sub getSNP{ my (@genes) = @_;
	my $geneStr;
	my @list;

	my $i;
	for($i = 0; $i <=  int(length(@genes) / 200); $i++){
		$geneStr .= join(" ", @genes[$i*200 .. ($i+1)*200]);

		my $uri = URI->new("http://www.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi");
		$uri->query_form('dbfrom' => 'gene', 'db' => 'snp', 'id' => $geneStr);
	
		my $parser = new XML::Simple;
		my $data = $parser->XMLin(LWP::Simple::get($uri->as_string));
	
		if(@genes > 1){
			foreach (@{$data->{LinkSet}{LinkSetDb}}){
				if (ref($_->{Link}) eq "ARRAY") {
					foreach (@{$_->{Link}}){
						push(@list, $_->{Id});
					}
				}else{
					push(@list, $_->{Link}{Id});	
				}
		        }
		}else{
			if (ref($data->{LinkSet}{LinkSetDb}{Link}) eq "ARRAY") {
				foreach (@{$data->{LinkSet}{LinkSetDb}{Link}}){
					push(@list, $_->{Id});
				}
			}else{
                                push(@list, $data->{LinkSet}{LinkSetDb}{Link}{Id});	
			}
			
		}
	}
       	return &uniqueArray(\@list);
}

sub uniqueArray
{
	my @array;
	my $list=shift;
	my %seen = (); 
	my $item;
	foreach $item (@$list) { $seen{$item}++; } 
	@array = keys %seen;     
	return @array;	
	
}

sub sparqlQuery{ my ($endpoint, $queryString) = @_;
	print "\n\n" . $endpoint . " : " . $queryString . "\n\n";
	# construct URL
	my $uri = URI->new($endpoint);
	$uri->query_form('query' => $queryString, 'format' => 'application/rdf+xml');
	# parser for rdfxml
	my $parser = RDF::Trine::Parser->new( 'rdfxml' );
	#print($parser);
	# construct perl trine triple store
	my $tripleStore = RDF::Trine::Model->new();
	# read and parse the URL
  	#print($uri->as_string);
	$parser->parse_url_into_model( $uri->as_string, $tripleStore );
	return $tripleStore;
}


sub unionQuery{ my ($query, $list, $gene) = @_;
	$query =~ m#.*where {(.*)}#g;
	#print "\n$query\n";
	my $templ = $1;
	#print "\n$templ\n";
	my $where;
	
	if (!@$list) {
		$where = substr($templ, 0, length($templ) - 25) ;
		#print $where;
	}
	else
	{
		foreach (@$list) {
			#print $_."\n";
			#my $block = $templ;
			#$block =~ s#$gene#\<$_\>#g;
			$where .= "{".$templ . 
				" filter ($gene = <http://bio2rdf.org/geneid:" . $_ .
				">) .} union";
			#$construct .= "?part1 ?inter ?part2 . ";
		}
		substr($where,length($where)-6,6,"");
		foreach (@$list) {
			$where .= " filter (?gene2 != <http://bio2rdf.org/geneid:" . $_ . ">) .";
		}
	}
	#return "construct {?part1_title ?inter ?part2_title .} where { $where }";  
	#return "construct {?inter a <http://bio2rdf.org/bind#Interaction> .} where { $where }"; 
	#return "select ?gene1 ?gene2 where { $where } limit 5";  
	$query =~ s#where {.*}#where {$where}#g;
	return $query;
}


sub getGene
{
	my $triples = shift;
	my $iterator = $triples->as_stream;
	my @genes;
	while (my $row = $iterator->next) {
		#print Dumper($row);
 		#print $row->as_string."\n";
		#print "\nSubject:\n".$row->subject."\nPredicate:\n".$row->predicate."\nObject:\n".$row->object."\n";
		#print OUT $row->as_string."\n";
		#print OUT "\nSubject:\n".$row->subject."\nPredicate:\n".$row->predicate."\nObject:\n".$row->object."\n";
	
		if ($options->{database} eq "omim") {
			$row->object=~m#\"(\d*)\"\^\^\<http\:\/\/#;
			push(@genes, $1);
		}
		else {
			$row->subject=~ m#geneid\:(\d*)\>#;
			push(@genes, $1);
			$row->object =~ m#geneid\:(\d*)\>#;
			push(@genes, $1); 
		}
	}
	@genes=uniqueArray(\@genes);
	#print @genes . " gene(s) were found\n\n";          

	return @genes;
}

#main $#ARGV @ARGV %ENV



GetOptions($options,
	'help|?', 
	'man', 
	'endpoint=s',
	'file=s',
	'database=s',
	'list=s',
	'word',
	'ID',
	'outfile=s');


pod2usage(0) if $options->{'help'};
pod2usage(-exitstatus => 0, -verbose => 2) if $options->{'man'};



my @list;
my $triples;
my @genes;

if($options->{'database'}) {
        open (LIST, "<", $options->{'list'}) or die("error");
	@list = <LIST>;
	chomp @list;
	close LIST;
	my $flag = $options->{word} ? 1 : 0;
	#print @list . "\n";
	if($options->{'database'} ne "geneid"){
        	switch($options->{database})
		{
			case "interaction" {$triples=sparqlQuery("http://geneid.bio2rdf.org/sparql", interaction(\@list));}
			case "omim" {$triples=sparqlQuery("http://hgnc.bio2rdf.org/sparql", omim(\@list,$flag));}
			case "go" {$triples=sparqlQuery("http://geneid.bio2rdf.org/sparql", go(\@list,$flag));}
			case "kegg" {$triples=sparqlQuery("http://geneid.bio2rdf.org/sparql", kegg(\@list));}
			else {print "Bad database\n";}
		}
		@genes=getGene($triples);      	
	}
	else {
		@genes=@list;
	}
}
else{
	local $/ = undef;               
	my $query;
	open(FILE, "<", $options->{file} );
	$query = <FILE>;
	close FILE;
	$query =~ s/\n/ /g;
	$triples=sparqlQuery($options->{endpoint},$query);
	
	my $iterator = $triples->as_stream;

	open (OUT, ">", $options->{'outfile'} . ".res");
        while (my $row = $iterator->next) {
	
		print OUT "\nSubject:\n".$row->subject."\nPredicate:\n".$row->predicate."\nObject:\n".$row->object."\n";
        }
       	close OUT;
       	exit (0);
}



open (OUT, ">", $options->{'outfile'} . ".list");


foreach(@genes){
	print OUT $_ . "\n";	
}

close OUT;

open (SNP, ">", $options->{'outfile'} . ".snps");
foreach(getSNP(@genes)){
	print SNP $_ . "\n";
}
close SNP;

exit(0);

__END__

=head1 NAME

B<RDF queries>
   
=head1 SYNOPSIS

perl rdf.pl B<-endpoint=>'http://example.com/sparql' B<-file=>'biogrid.qur' 
B<-outfile=>'outfile.txt'


perl rdf.pl B<-database=>'go' B<-list=>'example_2.list' B<-word> 
B<-outfile=>'outfile.txt'


 Options:
   -help            brief help message
   -man             full documentation
   -file 	    file with query
   -endpoint        url of data
   -database	    intalled query (geneid, interaction, kegg, go, omim)
   -list	    a list for searching
   -word	    if words in the list file
   -ID              if IDs in the list file
   -outfile         file for the results

=head1 OPTIONS

=over 14

=item B<-help>

Print a brief help message and exits.

=item B<-man>

Prints the manual page and exits.

=item B<-file>

The file contains your own query and the program gets results for the query 
without changing.

=item B<-endpoint>

The url of the server with data.

=item B<-database>

There are three queries already exist: for Gene, for Gene Ontology, and for OMIM. 
All of this queries search for some interaction between genes. 

Write B<'go'> if you have a list of B<GO terms or words>, that could be found in GO 
database. 

Write B<'omim'> if you have a list of B<omim gene IDs>. 

Write B<'geneid'> if you have a list of B<gene IDs>.

Write B<'interaction'> if you have a list of B<gene IDs> and want to know interacting genes.

Write B<'kegg'> if you have a list of B<KEGG pathway IDs>.

=item B<-list>

The file with list of terms which you need to request.

=item B<-word>

If you have the list of words in the file.

=item B<-ID>

If you have the list of IDs in the file.

=item B<-outfile>

The outfile contains a list of interacting geneIDs. 

=back

=head1 DESCRIPTION

B<rdf.pl> works in two ways:

1) read the given input B<file> with query and coresponding B<endpoint>, and give file with 
results;

2) request one of five existing query to B<databases>: SNP database, KEGG, Gene, Gene Ontology,
and OMIM and return a list of related genes and SNPs. The genes from input data are 
also in output. Results could be written in two B<outfile> as a list of gene IDs and 
a list of SNPs.


=cut