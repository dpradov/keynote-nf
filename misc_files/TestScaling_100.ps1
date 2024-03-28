# C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe -ExecutionPolicy Bypass -File "TestScaling_100.ps1"

# $scaling = 0 : 100% (predeterminado)
# $scaling = 1 : 125%
# $scaling = 2 : 150%
# $scaling = 3 : 175%
param($scaling = 0)

$source = @'
[DllImport("user32.dll", EntryPoint = "SystemParametersInfo")]
public static extern bool SystemParametersInfo(
uint uiAction,
uint uiParam,
uint pvParam,
uint fWinIni);
'@

$apicall = Add-Type -MemberDefinition $source -Name WinAPICall -Namespace SystemParamInfo –PassThru
$apicall::SystemParametersInfo(0x009F, $scaling, $null, 1) | Out-Null

