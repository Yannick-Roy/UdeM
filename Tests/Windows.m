
for i = 20:40
    moy = mean(EEG.data(i * 220 : i * 220 + 220));
    EEG.data(i * 220 : i * 220 + 220) = EEG.data(i * 220 : i * 220 + 220) - moy;
    figure; pop_spectopo(EEG, 1, [i * 1000  i * 1000 + 1000], 'EEG' , 'percent', 100, 'freqrange',[2 80],'electrodes','off');
    pause(1)
end

figure; pop_spectopo(EEG, 1, [2000  55000], 'EEG' , 'percent', 100, 'freqrange',[2 80],'electrodes','off');