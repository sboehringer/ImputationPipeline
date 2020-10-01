#
#	BatchQueue.pm
#Fri Jul  3 10:53:35 2015

{	# abstract batch queueing class
	package BatchQueue;
	use Moose;

	# submit a command to the batch queue
	#	return hash with return code, jobid
	sub submit { die 'abstract method' }
	# query job ids (jids) currently queued
	sub queuedJobs { die 'abstract method' }
	no Moose;
}

{	# a synchronous batch queue using /bin/sh
	package BatchQueueSh;
	use Moose;
	extends 'BatchQueue';

	use TempFileNames;

	sub submit { my ($self, $cmd, $logLevel, %o) = @_;
		my $ret = TempFileNames::System($cmd, $logLevel, undef, {%o});
Log("Return code:$ret");
		return { return => $ret, jid => undef };
	}
	sub queuedJobs { return (); }
	no Moose;
}

{	# sun grid engine batch queueing class
	package BatchQueueSGE;
	use Moose;
	extends 'BatchQueue';

	use TempFileNames;

	has 'jidFile', is => 'ro', isa => 'Str', default => sub { 
		tempFileName("/tmp/sge_jidFile_$ENV{USER}") };

	# submit a command to the batch queue
	# o: options: waitJids
	#<i> implement jidReplace in qsub.pl
	sub submit { my ($self, $cmd, $logLevel, %o) = @_;
		$logLevel = 4 if (!defined($logLevel));
		my $wait = defined($o{waitJids}) && int(@{$o{waitJids}})?
			"--waitForJids '". join(',', @{$o{waitJids}}). "'": '';
		my $storeJids = defined($self->jidFile)? '--jidReplace '. $self->jidFile: '';
		$cmd = "qsub.pl $wait $storeJids --unquote -- ". qs($cmd);
		TempFileNames::System($cmd, $logLevel, undef, {%o});
		return { jid => defined($self->jidFile)? int(readFile($self->jidFile)): -1 };
	}
	# query job ids (jids) currently queued
	sub queuedJobs {
		my @jobs;
		my $xml = substr(Set::firstDef(`which xml 2>/dev/null`, `which xmlstarlet 2>/dev/null`), 0, -1);
		if (defined($xml)) {
			@jobs = (`qstat -u \\* -xml | $xml sel -t -m '//JB_job_number' -v 'text()' -o ' '`
				=~ m{(\d+)}sog);
		} else {
			my $jobs = TempFileNames::System("qstat -u '*' | tail -n+3 | cut -d ' ' -f 1 -",
				7, undef, { returnStdout => 'YES'})->{output};
			@jobs = split(/\n/, $jobs);
		}
		return @jobs;
	}
	no Moose;
}

1;
