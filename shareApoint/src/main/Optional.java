package main;

public class Optional<T> {

    private final boolean hasValue;
    private final T value;
    
    public Optional() { 
        this.hasValue = false;
        this.value = null;
    }
    
    public Optional(T value) {
        this.hasValue = true;
        this.value = value;
    }
    
    public boolean hasValue() {
        return this.hasValue;
    }
    
    public T get() {
        if(hasValue) return value;
        else throw new IllegalStateException(" can't get value");
    }
    
    public static <T> Optional<T> none() {
        return new Optional<T>();
    }
    
    public static <T> Optional<T> some(T value) {
        return new Optional<T>(value);
    }
    
    @Override
    public String toString() {
        if(!hasValue) return "NONE";
        else return "Some(" + value + ")";
    }
    
}
