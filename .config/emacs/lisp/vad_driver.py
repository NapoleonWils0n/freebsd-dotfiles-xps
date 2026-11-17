#!/usr/local/bin/python3
import sys
import numpy as np
import onnxruntime as ort
import os

class SileroVADONNX:
    def __init__(self, model_path, sample_rate=16000):
        self.sample_rate = sample_rate
        opts = ort.SessionOptions()
        opts.inter_op_num_threads = 1
        opts.intra_op_num_threads = 1
        self.session = ort.InferenceSession(model_path, providers=['CPUExecutionProvider'])
        
        # Silero VAD state tensor: [2, 1, 128]
        self.state = np.zeros((2, 1, 128), dtype=np.float32)
        # Context buffer: last 64 samples of the previous frame (initialized to zeros)
        self.context = np.zeros((1, 64), dtype=np.float32)

    def __call__(self, chunk_pcm32):
        if len(chunk_pcm32.shape) == 1:
            chunk_pcm32 = np.expand_dims(chunk_pcm32, axis=0)

        # Prepend 64-sample context to the 512-sample current frame -> total 576 samples
        model_input = np.hstack([self.context, chunk_pcm32])

        sr = np.array(self.sample_rate, dtype=np.int64)
        inputs = {
            'input': model_input,
            'state': self.state,
            'sr': sr
        }
        
        out, self.state = self.session.run(None, inputs)
        
        # Save the last 64 samples of the current chunk as context for the next iteration
        self.context = chunk_pcm32[:, -64:]
        
        return out[0][0]

def main():
    model_path = os.path.expanduser("~/.local/share/silero/silero_vad.onnx")
    if not os.path.exists(model_path):
        print(f"Error: Silero ONNX model not found at {model_path}", file=sys.stderr)
        sys.exit(1)

    vad = SileroVADONNX(model_path)
    chunk_size_bytes = 1024  # 512 samples * 2 bytes per sample (16-bit int)

    triggered = False
    silence_frames = 0
    silence_threshold = 15  # ~500ms of consecutive low probability before END

    while True:
        raw_data = sys.stdin.buffer.read(chunk_size_bytes)
        if not raw_data:
            break
            
        if len(raw_data) < chunk_size_bytes:
            raw_data = raw_data + b'\x00' * (chunk_size_bytes - len(raw_data))

        audio_chunk = np.frombuffer(raw_data, dtype=np.int16).astype(np.float32) / 32768.0
        speech_prob = vad(audio_chunk[:512])

        # Debug print to stderr to verify probabilities rise when speaking
        print(f"Prob: {speech_prob:.4f}", file=sys.stderr, flush=True)

        if speech_prob > 0.3:
            silence_frames = 0
            if not triggered:
                triggered = True
                print("START", flush=True)
        else:
            if triggered:
                silence_frames += 1
                if silence_frames >= silence_threshold:
                    triggered = False
                    silence_frames = 0
                    print("END", flush=True)

if __name__ == "__main__":
    main()
