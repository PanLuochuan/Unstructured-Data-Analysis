#### Pan Luochuan
#### p136608
### Task 2 : Audio data analysis.


library(tuneR)
library(seewave)
library(fftw)
library(tidyverse)
library(wrassp)
library(ggplot2)
library(pracma)

### Load data
one = "D:/ukm.ppt/非结构数据/project3/Audio_Files/Major/Major_0.wav"
audio1 <- readWave(one)  %>% tuneR::normalize(unit = c("1"), center = FALSE, rescale = FALSE)# Major_0.wav
play(audio1)

two = "D:/ukm.ppt/非结构数据/project3/Audio_Files/Major/Major_2.wav"
audio2 <- readWave(two)  %>% tuneR::normalize(unit = c("1"), center = FALSE, rescale = FALSE) # Major_2.wav
play(audio32)

three = "D:/ukm.ppt/非结构数据/project3/Audio_Files/Minor/Minor_0.wav"
audio3 <- readWave(three)  %>% tuneR::normalize(unit = c("1"), center = FALSE, rescale = FALSE)# # Minor_0.wav
play(audio3)

four = "D:/ukm.ppt/非结构数据/project3/Audio_Files/Minor/Minor_2.wav"
audio4 <- readWave(four)  %>% tuneR::normalize(unit = c("1"), center = FALSE, rescale = FALSE) # Minor_2.wav
play(audio3)

audio_files <- list(audio1, audio3, audio2, audio4)
names(audio_files) <- c("Major_0", "Minor_0", "Major_2", "Minor_2")

# Output Summary
for (i in seq_along(audio_files)) {
  audio <- audio_files[[i]]
  audio_name <- names(audio_files)[i]
  
  print(paste("Audio Name:", audio_name))
  print(summary(audio))
}


### Plotting time and frequency domain graphs
for (i in seq_along(audio_files)) {
  audio <- audio_files[[i]]
  audio_name <- names(audio_files)[i]
  
  # Extract audio signal and sampling rate
  signal <- audio@left
  fs <- audio@samp.rate
  
  # Fourier transform and frequency domain analysis
  N <- length(signal)
  signal_f <- fft(signal)
  time <- seq(0, N/fs, length.out = N)
  N_half <- as.integer(N / 2)
  
  # Extract half of the spectrum to calculate the amplitude
  signal_f_onesided <- 2 * abs(signal_f[1:N_half]) / N
  y_freq <- seq(0, fs/2, length = N_half)
  
  par(mfrow=c(2, 2), mar=c(3, 3, 3, 3))
  
  # Time Domain Graph
  plot(time, signal, type = 'l', main = paste("Sound Wave in Time Domain (No Zoom) -", audio_name), xlab = "Time [sec]", ylab = "Amplitude")
  
  # Frequency Domain Plot
  plot(y_freq, signal_f_onesided, type = 'l', main = paste("Sound Wave in Frequency Domain (No Zoom) -", audio_name), xlab = "Frequency [Hz]", ylab = "Magnitude")
  
  # Zoom in on the time domain
  plot(time[(N/2):(N/2+480)], signal[(N/2):(N/2+480)], type = 'l', main = paste("Sound Wave in Time Domain (Zoomed) -", audio_name), xlab = "Time [sec]", ylab = "Amplitude")
  
  # Frequency domain zoom
  plot(y_freq[1:5000], signal_f_onesided[1:5000], type = 'l', main = paste("Sound Wave in Frequency Domain (Zoomed) -", audio_name), xlab = "Frequency [Hz]", ylab = "Magnitude")
}

### STFT,iSTFT
windows(width=10,height=7)
wl <- 1024  
ovlp <- 75 
for (i in seq_along(audio_files)) {
  audio_name <- names(audio_files)[i]
  audio <- audio_files[[i]]
  # STFT
  stft_data <- spectro(audio, wl=wl, ovlp=ovlp, plot=FALSE,dB=NULL, norm=FALSE, complex=TRUE)$amp
  # iSTFT
  res_audio <- istft(stft_data, ovlp=ovlp, wn="hanning", wl=wl, f=audio@samp.rate, out="Wave")
  
  spectro(res_audio, flim=c(0, 10), main=paste("Reconstructed Spectrogram of", audio_name))
  cat("Press [Enter] to continue:\n")
  readline()
}
dev.off()

### Harmonic analysis
for (i in seq_along(audio_files)) {
  audio <- audio_files[[i]]
  audio_name <- names(audio_files)[i]
  
  signal <- audio@left
  fs <- audio@samp.rate
  N <- length(signal)
  signal_f <- fft(signal)
  signal_f_onesided <- abs(signal_f[1:(N/2 + 1)]) / N 
  freq_vector <- seq(0, fs/2, length.out = length(signal_f_onesided))
  
  # Finding the peak
  threshold <- max(signal_f_onesided) * 0.3
  
  peaks <- which(signal_f_onesided > threshold)
  
  plot_data <- data.frame(Frequency = freq_vector, Amplitude = signal_f_onesided)
  peaks_data <- data.frame(Frequency = freq_vector[peaks], Amplitude = signal_f_onesided[peaks])
  
  # Spectrogram
  max_amplitude <- max(plot_data$Amplitude, na.rm = TRUE)
  min_amplitude <- min(plot_data$Amplitude, na.rm = TRUE)
  
  p <- ggplot(plot_data, aes(x = Frequency, y = Amplitude)) +
    geom_line(color = "darkblue") +
    geom_point(data = peaks_data, aes(x = Frequency, y = Amplitude), color = "darkred", size = 3, shape = 4) +
    xlim(c(0, 1000)) +  
    ylim(c(min_amplitude, max_amplitude)) +  
    ggtitle(paste("Frequency Spectrum with Harmonics for", audio_name)) +
    xlab("Frequency (Hz)") +
    ylab("Amplitude") +
    theme_minimal()
  
  print(p)
}




