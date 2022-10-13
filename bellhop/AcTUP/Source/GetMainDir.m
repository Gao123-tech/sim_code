function Dir = GetMainDir()
%Returns the main directory for the propagation program files

AllDir = GetAcDirectoryInfo;
Dir = AllDir.MainWork;