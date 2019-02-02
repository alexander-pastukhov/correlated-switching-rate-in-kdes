# Correlated switching rate in kinectic-depth effect displays

Data and analysis for the study on (un)correlated switching rate in KDE and NC displays.

The full dataset is in **Data** folder. Supplementary videos are in **Videos** folder. For Complete analysis please refer to [Analysis.Rmd](Analysis.Rmd) file or to static Github version of it [Analysis.md](Analysis.md).

## CSV-format

* **Session**  session timestamp in the form of _year-month-day-hour-minute-second_.
* **Participant** Unique participant ID.
* **Block** Block index.
* **Condition** Block condition, can be either _Passive_, _Speed-up_, or _Slow-down_.
* **Display** Block display, can be either _KDE -45_ (clockwise rotated kinetic-depth effect), _KDE 45_ (counterclockwise rotated kinetic-depth effect), or _NC_ (Necker cube lattice).
* **OnsetDelay** Randomized delay before the stimulus onset in seconds.
* **Percept** Perceptual reports (_left_, _right_, or _unclear_) or the _end of the block_ message.
* **Time** Time of the perceptual report or of the end of the block relative to the block start, in seconds.

## License
All data (and associated content) is licensed under the [CC-By Attribution 4.0 International License](https://creativecommons.org/licenses/by/4.0/). All code is licensed
under the [MIT License](http://www.opensource.org/licenses/mit-license.php).
