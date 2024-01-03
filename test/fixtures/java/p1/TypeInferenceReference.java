package p1;

import java.util.List;

public class TypeInferenceReference {
    public void forEach() {
        List.of("a").forEach(v -> System.out.println(v));
    }

    public void forEachWithReference() {
        var list = List.of("a");
        list.forEach(v -> System.out.println(v));
    }
}
