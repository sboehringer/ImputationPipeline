Log.setLevel(4);
Parallelize_config__ = list(
	max_depth = 3, parallel_count = 64, offline = T,
#	sourceFiles = 'RgenericAll.R',
	sourceFiles = c(),
	backends = list(
	snow = list(
		localNodes = 8, splitN = 2, stateDir = 'QSUB_LOCAL_DIR',
		parallel_count = 64
	),
	local = list(
		stateDir = 'QSUB_LOCAL_DIR', parallel_count = 64

	),
	`local-grouped` = list(
		backend = 'local',
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = 'QSUB_LOCAL_DIR'
	),
	`ogs-1` = list(
		backend = 'OGS',
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = 'QSUB_LOCAL_DIR',
		qsubRampUpMemory = '16G',
		qsubParallelMemory = '4G',
		qsubOptions = sprintf('--queue all.q --logLevel %d', Log.level()),
		doNotReschedulde = T,
		doSaveResult = T
	),
	`ogs-2` = list(
		backend = 'OGS',
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = 'QSUB_LOCAL_DIR',
		qsubRampUpMemory = '16G',
		qsubParallelMemory = '4G',
		qsubOptions = sprintf('--queue all.q --logLevel %d', Log.level()),
		doSaveResult = T
	),
	`ogs-3` = list(
		backend = 'OGSremote',
		remote = 'pingu@localhost:tmp/remote/test',
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = 'QSUB_LOCAL_DIR',
		qsubRampUpMemory = '16G',
		qsubParallelMemory = '4G',
		qsubOptions = sprintf('--queue all.q --logLevel %d', Log.level()),
		doSaveResult = T
	),
	`ogs-shark-subordinate` = list(
		backend = 'OGSremote',
		remote = 'QSUB_REMOTE_DIR',
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = 'QSUB_LOCAL_DIR',
		qsubOptions = sprintf('--queue subordinate.q --checkpointing --logLevel %d --memory QSUB_MEMORY', Log.level() + 1),
		qsubRampUpMemory = '16G',
		qsubParallelMemory = '4G',
		ssh_source_file = '~/.env_profile',
		doSaveResult = T
	),
	`ogs-shark-all-big` = list(
		backend = 'OGSremote',
		remote = 'QSUB_REMOTE_DIR',
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = sprintf('%s/tmp/parallel_states', Sys.getenv('HOME')),
		qsubOptions = sprintf('--queue all.q --logLevel %d --memory QSUB_MEMORY', Log.level() + 2),
		qsubRampUpMemory = '32G',
		qsubParallelMemory = '4G',
		ssh_source_file = '~/.env_profile',
		doSaveResult = T
	),
	`ogs-shark-all` = list(
		backend = 'OGSremote',
		remote = 'QSUB_REMOTE_DIR',
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = sprintf('%s/tmp/parallel_states', Sys.getenv('HOME')),
		qsubOptions = sprintf('--queue all.q --logLevel %d --memory QSUB_MEMORY', Log.level() + 2),
		qsubRampUpMemory = '8G',
		qsubParallelMemory = '3G',
		ssh_source_file = '~/.env_profile',
		doSaveResult = T,
		parallel_count = 500
	),
	`ogs-hgx` = list(
		backend = 'OGSremote',
		remote = sprintf('localhost:%s/tmp/parallel_jobs', Sys.getenv('HOME')),
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = 'QSUB_LOCAL_DIR',
		qsubOptions = sprintf('--queue all.q --logLevel %d --memory QSUB_MEMORY', Log.level() + 1),
		qsubRampUpMemory = '8G',
		qsubParallelMemory = '3G',
		doSaveResult = T,
		parallel_count = 100
	),
	`ogs-hgx-debug` = list(
		backend = 'OGS',
		freezerClass = 'LapplyGroupingFreezer',
		stateDir = 'QSUB_LOCAL_DIR',
		qsubOptions = sprintf('--queue all.q --logLevel %d --memory QSUB_MEMORY', Log.level() + 1),
		qsubRampUpMemory = '16G',
		qsubParallelMemory = '4G',
		doSaveResult = T,
		parallel_count = 100
	)

));
#	parallelize_initialize(Parallelize_config__, backend = backend, sourceFiles = c('inverseRegression.R', 'Rgenetics.R'), force_rerun = force_rerun, parallel_count = N);
# 
# Parallelize_config__ = mergeDictToDict(list(`QSUB_REMOTE_DIR` = 'sboehringer@shark:/data/MolEpi/users/sboehringer/parallel'), Parallelize_config__);

Parallelize_config__ = mergeDictToDict(list(
	`QSUB_REMOTE_DIR` = 'sboehringer@sharkr:/data/MolEpi/users/sboehringer/parallel',
	`QSUB_LOCAL_DIR` = sprintf('%s/tmp/parallel_states', Sys.getenv('HOME'))
	), Parallelize_config__
);
