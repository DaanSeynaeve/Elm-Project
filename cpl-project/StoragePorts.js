// local storage
// localStorage.removeItem('elm-manager-state');
var storedState = localStorage.getItem('elm-cpl-project-dseynaeve-state');
var startingState = storedState ? JSON.parse(storedState) : null;
// console.log('loading state...');
// console.log(startingState);

// ports
var manager = Elm.fullscreen(Elm.Main, { getStorage: startingState });
manager.ports.setStorage.subscribe(function(state) {
    localStorage.setItem('elm-cpl-project-dseynaeve-state', JSON.stringify(state));
    // console.log('storing state...');
    // console.log(state);
});
