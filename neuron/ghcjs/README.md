Run bin/build-impulse.js to rebuild impulse.js by compiling the sources under impulse/ folder.

This is done as part of neuron build (and in CI), because we don't want users to setup reflex-platform cache and do full GHCJS builds.

When should we rebuild impulse.js? Anytime HTML/CSS of neuron changes.