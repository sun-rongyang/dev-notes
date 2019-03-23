# Linux notes

## Distributions

### Clear Linux
#### Installation (From Mac or Linux)
- Download image file

Download the `img` file from https://download.clearlinux.org/image/ . If you can let your machine connect to the Internet directly, download the file end with `installer.img.xz`. Or if you must through a verification from a webpage, download the file end with `live-desktop-beta.img.xz`.

- Burn the `img` onto a USB derive



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
 
