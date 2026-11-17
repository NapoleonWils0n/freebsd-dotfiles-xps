#!/usr/local/bin/python3
import sys
import subprocess
import numpy as np
import onnxruntime as ort
from datetime import datetime, timedelta
import os

class SileroVADONNX:
    def __init__(self, model_path, sample_rate=16000):
        self.sample_rate = sample_rate
        opts = ort.SessionOptions()
        opts.inter_op_num_threads = 1
        opts.intra_op_num_threads = 1
        opts.execution_mode = ort.ExecutionMode.ORT_SEQUENTIAL
        self.session = ort.InferenceSession(model_path, opts, providers=['CPUExecutionProvider'])
        self.state = np.zeros((2, 1, 128), dtype=np.float32)
        self.context = np.zeros((1, 64), dtype=np.float32)

    def __call__(self, chunk_pcm32):
        if len(chunk_pcm32.shape) == 1:
            chunk_pcm32 = np.expand_dims(chunk_pcm32, axis=0)

        model_input = np.hstack([self.context, chunk_pcm32])
        sr = np.array(self.sample_rate, dtype=np.int64)
        inputs = {
            'input': model_input,
            'state': self.state,
            'sr': sr
        }
        out, self.state = self.session.run(None, inputs)
        self.context = chunk_pcm32[:, -64:]
        return out[0][0]

def main():
    model_path = os.path.expanduser("~/.local/share/silero/silero_vad.onnx")
    if not os.path.exists(model_path):
        print(f"Error: Silero ONNX model not found at {model_path}", file=sys.stderr)
        sys.exit(1)

    vad = SileroVADONNX(model_path)
    
    # Stream raw audio chunks from OSS device using ffmpeg with gain
    ffmpeg_cmd = [
        "ffmpeg",
        "-hide_banner",
        "-fflags", "nobuffer",
        "-flags", "low_delay",
        "-f", "oss",
        "-i", "/dev/dsp3",
        "-af", "volume=5dB",
        "-ar", "16000",
        "-ac", "1",
        "-f", "s16le",
        "pipe:1"
    ]
    
    process = subprocess.Popen(ffmpeg_cmd, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    stream_start_time = datetime.now()
    
    chunk_size_bytes = 1024  # 512 samples * 2 bytes per sample
    triggered = False
    silence_frames = 0
    silence_threshold = 15  # ~500ms of consecutive low probability

    def format_iso_with_offset(seconds):
        event_time = stream_start_time + timedelta(seconds=seconds)
        return event_time.astimezone().isoformat(timespec='milliseconds')

    try:
        while True:
            raw_data = process.stdout.read(chunk_size_bytes)
            if not raw_data:
                break
                
            if len(raw_data) < chunk_size_bytes:
                raw_data = raw_data + b'\x00' * (chunk_size_bytes - len(raw_data))

            audio_chunk = np.frombuffer(raw_data, dtype=np.int16).astype(np.float32) / 32768.0
            speech_prob = vad(audio_chunk[:512])
            current_time_sec = (datetime.now() - stream_start_time).total_seconds()

            if speech_prob > 0.5:
                silence_frames = 0
                if not triggered:
                    triggered = True
                    print(f"START {format_iso_with_offset(current_time_sec)}", flush=True)
            else:
                if triggered:
                    silence_frames += 1
                    if silence_frames >= silence_threshold:
                        triggered = False
                        silence_frames = 0
                        print(f"END {format_iso_with_offset(current_time_sec)}", flush=True)

    except KeyboardInterrupt:
        pass
    finally:
        process.terminate()

if __name__ == "__main__":
    main()
