#
#	Rinitialize.R
#

Rprofile = sprintf('%s/.Rprofile', Sys.getenv('HOME'));
if (file.exists(Rprofile)) source(Rprofile);
loadLibraries();
