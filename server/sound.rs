//! Nonblocking, best-effort user notification sounds.

use std::path::Path;
use std::process::{Command, Stdio};

const READY_SOUND : &str = "/home/sound/beep-glorious.wav";
const HARSH_SOUND : &str = "/home/sound/beep-harsh.wav";

pub fn play_ready_sound_in_background () {
  play_sound_in_background (READY_SOUND, "ready");
}

pub fn play_harsh_sound_in_background () {
  play_sound_in_background (HARSH_SOUND, "harsh warning");
}

fn play_sound_in_background (
  path  : &'static str,
  label : &'static str,
) {
  if !Path::new (path) . exists () {
    tracing::debug! (sound = path, %label, "Sound not found; skipping playback");
    return; }
  std::thread::spawn (move || {
    let result = Command::new ("pw-play")
      . arg (path)
      . stdin (Stdio::null ())
      . stdout (Stdio::null ())
      . stderr (Stdio::null ())
      . status ();
    if let Err (error) = result {
      tracing::debug! (
        %error, sound = path, %label, "Sound playback failed"); }});
}
