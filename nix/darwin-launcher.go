package main

import (
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

	launcherConfig := filepath.Join(installDir, "../Resources/launcher-config.yaml")
	launcher := filepath.Join(installDir, "cardano-launcher")
	helper := filepath.Join(installDir, "../Resources/helper")

	if err = exec.Command(helper).Run(); err != nil {
		panic(err)
	}
	if err = exec.Command(launcher, "--config", launcherConfig).Run(); err != nil {
		panic(err)
	}
}
