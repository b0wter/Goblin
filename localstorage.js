function saveToLocalStorage(key, value) {
    // Messages from Elm to JS are one-way only!
    // There is no point in defining a return value.
    try {
        localStorage.setItem(key, value);
    }
    catch(ex) {
        console.error(`There was an error trying to save '${key}' - '${value}' to the local storage:`, ex);
    }
}

function loadFromLocalStorage(key) {
    return localStorage.getItem(key);
}

function removeFromLocalStorage(key) {
    // Messages from Elm to JS are one-way only!
    // There is no point in defining a return value.
    try {
        localStorage.removeItem(key);
    }
    catch(ex) {
        console.log(ex);
    }
}