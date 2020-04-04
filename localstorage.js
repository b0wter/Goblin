function saveToLocalStorage(key, value) {
    try {
        localStorage.setItem(key, value);
        return true;
    }
    catch(ex) {
        console.log(ex);
        return false;
    }
}

function loadFromLocalStorage(key) {
    return localStorage.getItem(key);
}

function removeFromLocalStorage(key) {
    try {
        localStorage.removeItem(key);
        return true;
    }
    catch(ex) {
        console.log(ex);
        return false;
    }
}