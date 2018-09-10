# Linux notes

## Unarchived

### HowTo

- How to suspend and resume a background process?

  1. Find its PID.
  2. Use `kill` command to suspend/resume it.

  ```bash
  # Send  SIGTSTP(recommend) or SIGSTOP to suspend.
  kill -TSTP $PID
  kill -STOP $PID
  # Send SIGCONT to resume.
  kill -CONT $PID
  ```

  