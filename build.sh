dotnet pack build.proj /p:BN=$1 /p:PR=$2
dotnet test tests/XRay.Tests -v n
