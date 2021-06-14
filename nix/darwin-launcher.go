package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

func main() {
	ex, err := os.Executable()
	if err != nil {
		panic(err)
	}

	installDir := filepath.Dir(ex)

	os.Setenv("PATH", fmt.Sprintf("%s:%s", installDir, os.Getenv("PATH")))

	launcherConfig := filepath.Join(installDir, "../Resources/launcher-config.yaml")
	helper := filepath.Join(installDir, "../Resources/helper")


	if err = exec.Command(helper).Run(); err != nil {
		panic(err)
	}
	if err = exec.Command("cardano-launcher", "--config", launcherConfig).Run(); err != nil {
		panic(err)
	}
}
