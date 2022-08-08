package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"syscall"
)

func main() {
	fmt.Fprintf(os.Stderr, "darwin-launcher: PID = %d\n", os.Getpid())

	ex, err := os.Executable()
	if err != nil {
		panic(err)
	}

	installDir := filepath.Dir(ex)

	os.Setenv("PATH", fmt.Sprintf("%s:%s", installDir, os.Getenv("PATH")))

	launcherConfigPath := filepath.Join(installDir, "../Resources/launcher-config.yaml")
	helperPath := filepath.Join(installDir, "../Resources/helper")

	helperCmd := exec.Command(helperPath)
	helperCmd.Stdout = os.Stdout
	helperCmd.Stderr = os.Stderr
	if err := helperCmd.Run(); err != nil {
		panic(err)
	}

	// Replace the current process (otherwise WDIO complains in end-to-end tests):
	img := filepath.Join(installDir, "cardano-launcher")
	argv := []string{"cardano-launcher", "--config", launcherConfigPath}
	env := os.Environ()
	if err := syscall.Exec(img, argv, env); err != nil {
		fmt.Println(err)
	}

	fmt.Fprintf(os.Stderr, "this won’t happen\n")
}
