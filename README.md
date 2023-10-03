# ej (Editor Jumper)

This is a (WIP) program designed to make jumping to a particular line in an editor based on, for example, a compiler error, faster. A goal is to stay independent of the particular editor used, by assuming that there is some shell command that takes a filename and line number, and opens the editor at the specified location.