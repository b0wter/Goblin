function saveToLocalStorage(key, value) {
    // Messages from Elm to JS are one-way only!
    // There is no point in defining a return value.
    console.debug(`Trying to save something to local storage using the key '${key}.`);
    try {
        localStorage.setItem(key, value);
    }
    catch(ex) {
        console.error(`There was an error trying to save '${key}' - '${value}' to the local storage:`, ex);
    }
}

function loadFromLocalStorage(key) {
    console.debug(`Trying to load something from local storage using the key '${key}'.`);
    return localStorage.getItem(key);
}

function removeFromLocalStorage(key) {
    // Messages from Elm to JS are one-way only!
    // There is no point in defining a return value.
    console.debug(`Trying to remove something from local storage using the key '${key}'.`);
    try {
        localStorage.removeItem(key);
    }
    catch(ex) {
        console.log(ex);
    }
}

function doesLocalStorageKeyExist(key) {
    console.debug(`Trying to find the key ${key}' in local storage.`);
    try {
        const item = localStorage.getItem(key);
        return !(item === null);
    }
    catch(ex) {
        console.log(ex);
        return false;
    }
}